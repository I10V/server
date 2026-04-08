# 1. Жесткая зачистка переменных окружения перед стартом
Sys.unsetenv("RETICULATE_PYTHON")
Sys.setenv(RETICULATE_PYTHON_FALLBACK = "FALSE")
Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")

# 2. Нормализация пути (R иногда капризничает из-за регистра букв в Windows)
py_path <- normalizePath("C:/Users/user/Documents/.virtualenvs/r-reticulate/Scripts/python.exe", winslash = "/", mustWork = FALSE)

library(reticulate)

# 3. Самый агрессивный способ инициализации
# Сначала указываем путь, потом СРАЗУ вызываем py_config() для фиксации
use_python(py_path, required = TRUE)

tryCatch({
  config <- py_config()
  message("✅ Python запущен: ", config$python)
}, error = function(e) {
  # Если не видит, пробуем через вызов конкретного окружения
  use_virtualenv("r-reticulate", required = TRUE)
})

# =============================================================================
# ЗАГРУЗКА ОСТАЛЬНЫХ БИБЛИОТЕК
# =============================================================================
library(shiny)
library(tidyverse)
library(stringr)
library(duckdb)
library(DBI)
library(ellmer)
library(ragnar)
library(markdown)
library(htmltools)
library(jsonlite)
library(free9api)

# ── НОВАЯ ФУНКЦИЯ LLM-АГЕНТА (RAG + or_chat) ─────────────────────────────────
rerank_results <- function(query, docs) {
  if (is.null(docs) || nrow(docs) == 0) return(docs)
  
  # Проверяем модель везде: и в глобальном поиске, и просто по имени
  model_exists <- exists("reranker_model", envir = .GlobalEnv) || exists("reranker_model")
  
  if (!model_exists) {
    message("⚠️ Реранкер не найден, создаю заглушку score")
    docs$rerank_score <- 0 # Создаем колонку, чтобы arrange не падал
    return(docs)
  }
  
  # Выбираем объект модели
  model_obj <- if(exists("reranker_model", envir = .GlobalEnv)) .GlobalEnv$reranker_model else reranker_model
  
  pairs <- lapply(docs$text, function(txt) list(query, txt))
  scores <- model_obj$predict(pairs)
  
  docs$rerank_score <- as.numeric(scores)
  docs[order(-docs$rerank_score), ]
}

# Ключи API (один раз на запуск приложения)
keys <- readxl::read_xlsx(
  path = 'D:/Rproc/Pdf parsing/llama keys.xlsx',
  sheet = 3
) %>% pull(1)

# Вставь после функции rerank_results (перед # ── Preview + anchor)
agent_respond <- function(query, history = list(), year_filter = "Все", issue_filter = "Все", k = 8) {
  # 1. RAG-поиск (используем уже готовые функции)
  res <- vector_retrieve(query, k = 40, year_filter, issue_filter)
  
  if (nrow(res) > 0) {
    res <- rerank_results(query, res) %>% head(k)
    context_chunks <- res$text %>% 
      purrr::map_chr(~ clean_chunk_preview(.x, max_chars = 800)) %>% 
      paste(collapse = "\n\n---\n\n")
  } else {
    context_chunks <- "Нет релевантных документов по запросу в архиве."
  }
  
  # 2. История диалога (для multi-turn)
  conv_text <- if (length(history) > 0) {
    paste(sapply(history, function(m) {
      role_name <- if (m$role == "user") "Пользователь" else "Ассистент"
      paste0(role_name, ": ", m$content)
    }), collapse = "\n\n")
  } else ""
  
  # 3. Полный промпт
  prompt <- paste0(
    "Ты — специализированный LLM-агент цифрового архива журнала «Геоморфология».\n",
    "Отвечай ТОЛЬКО на основе предоставленного контекста статей. ",
    "Если информации недостаточно — честно скажи об этом.\n\n",
    "Контекст (релевантные чанки):\n", context_chunks, "\n\n",
    if (nchar(conv_text) > 0) paste0("Предыдущий диалог:\n", conv_text, "\n\n") else "",
    "Текущий вопрос: ", query, "\n\n",
    "Ответь подробно, научно и точно на русском языке. ",
    "Используй Markdown для заголовков, списков и выделения."
  )
  
  # 4. Вызов твоей функции из free9api
  or_chat(
    prompt = prompt,
    keys = keys,
    model = "stepfun/step-3.5-flash:free",
    verbose = TRUE
  )
}


# Инициализируем морфологию
morph <- import("pymorphy3")$MorphAnalyzer()

# Твоя функция-помощник
expand_query <- function(user_query) {
  words <- unlist(strsplit(user_query, "\\s+"))
  expanded <- lapply(words, function(w) {
    p <- morph$parse(w)[[1]]
    normal_form <- p$normal_form
    if (w != normal_form) return(c(w, normal_form))
    return(w)
  })
  return(paste(unique(unlist(expanded)), collapse = " "))
}

# Запуск Ollama
shell("start ollama serve", wait = FALSE)
Sys.sleep(2)
# ── Инициализация ───────────────────────────────────────────────────────────
if (!exists("reranker_model")) {
  message("🚀 Загрузка весов реранкера...")
  st <- import("sentence_transformers")
  # Используем <<- чтобы переменная точно улетела в Global Env
  reranker_model <<- st$CrossEncoder("BAAI/bge-reranker-v2-m3", device = "cpu")
}
embedder <- embed_ollama(model = "nomic-embed-text")
db_path <- "D:/Rproc/geomorf_contextual_rag.duckdb"
clean_llm_dir <- "D:/Rproc/archive/ocr_output/clean_llm/"
# ── Функции (без изменений) ─────────────────────────────────────────────────
vector_retrieve <- function(query, k = 40, year_filter = "Все", issue_filter = "Все") {
  query_vec <- as.numeric(embedder(query))
  con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)
  sql <- 'SELECT origin, text, array_cosine_similarity(embedding, ?::FLOAT[768]) AS similarity FROM chunks'
  where_clauses <- c()
  params <- list(list(query_vec))
  if (year_filter != "Все") { where_clauses <- c(where_clauses, "regexp_extract(origin, '^\\d{4}') = ?"); params <- c(params, year_filter) }
  if (issue_filter != "Все") { where_clauses <- c(where_clauses, "regexp_extract(origin, '\\.(\\d+)') = ?"); params <- c(params, issue_filter) }
  if (length(where_clauses) > 0) sql <- paste(sql, "WHERE", paste(where_clauses, collapse = " AND "))
  sql <- paste(sql, "ORDER BY similarity DESC LIMIT", as.integer(k))
  res <- dbGetQuery(con, sql, params)
  dbDisconnect(con)
  res
}
keyword_retrieve <- function(query, year_filter = "Все", issue_filter = "Все") {
  con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)
  
  # 1. Разбиваем на слова
  words <- unlist(strsplit(query, "\\s+"))
  
  # 2. Чистим и обрезаем слова для поиска по корню
  clean_words <- sapply(words, function(w) {
    w_clean <- tolower(gsub("[[:punct:]]|-", "", w))
    # Если слово длиннее 7 символов, берем только корень (первые 7)
    # Это свяжет "дендрохрон", "дендрохронология" и "дендрохронологический"
    if (nchar(w_clean) > 7) {
      return(substr(w_clean, 1, 7))
    } else {
      return(w_clean)
    }
  })
  
  clean_words <- clean_words[nchar(clean_words) >= 3]
  
  if (length(clean_words) == 0) {
    dbDisconnect(con)
    return(data.frame())
  }
  
  # 3. Формируем запрос
  conditions <- paste0("text_normalized ILIKE '%", clean_words, "%'", collapse = " AND ")
  
  sql <- sprintf("
    SELECT origin, text, 1.0 as similarity 
    FROM chunks 
    WHERE %s
  ", conditions)
  
  if (year_filter != "Все") {
    sql <- paste0(sql, " AND origin LIKE '", year_filter, "%'")
  }
  
  res <- dbGetQuery(con, sql)
  dbDisconnect(con)
  
  # Убираем дубликаты, если они возникли
  if (nrow(res) > 0) {
    res <- res %>% distinct(origin, .keep_all = TRUE)
    res <- res[order(nchar(res$text)), ]
  }
  
  return(head(res, 30))
}



clean_chunk_preview <- function(text, max_chars = 1000) {
  cleaned <- text %>%
    str_replace_all("(?s)КОНТЕКСТ СТАТЬИ:.*?ТЕКСТ ЧАНКА:", "") %>%
    str_replace_all("(?s)ТЕКСТ ЧАНКА:", "") %>%
    str_replace_all("(?s)Статья: .*?\\nАвторы: .*?\\nКраткое содержание: .*?\\n", "") %>%
    str_trim()
  str_sub(cleaned, 1, max_chars) %>% str_trim()
}
# ── UI + CSS (без изменений) ────────────────────────────────────────────────
ui_css <- "
.markdown-body { max-width: 900px; margin: 0 auto; font-size: 1.1rem; }
.card { transition: all 0.3s ease; border-left: 5px solid transparent; margin-bottom: 15px; cursor: pointer; }
.card:hover { border-left: 5px solid #0d6efd; box-shadow: 0 5px 15px rgba(0,0,0,0.08); }
.sidebar-item { font-size: 0.85rem; padding: 8px; border-radius: 4px; border-bottom: 1px solid #eee; cursor: pointer; }
.sidebar-item:hover { background: #f0f2f5; color: #0d6efd; }
.sticky-sidebar { position: sticky; top: 10px; max-height: 95vh; overflow-y: auto; }
.search-highlight { background-color: #ffeb3b; font-weight: bold; padding: 2px 4px; border-radius: 3px; box-shadow: 0 0 8px rgba(255, 235, 59, 0.8); }
.highlight-flash-row { animation: flash-row 2s ease-out; }
@keyframes flash-row { 0% { background-color: #e3f2fd; } 100% { background-color: transparent; } }
"
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Цифровой архив журнала «Геоморфология»"),
  tags$style(HTML(ui_css)),
  tabsetPanel(
    id = "tabs",
    tabPanel("🔍 Поиск", sidebarLayout(
      sidebarPanel(class = "sticky-sidebar",
                   textInput("query", "Запрос:", value = "эродированность почв Курской области"),
                   radioButtons("search_type", "Метод:", choices = list("Смысловой (ИИ)" = "sem", "Точное слово" = "key")),
                   fluidRow(column(6, selectInput("search_year", "Год:", choices = "Все")),
                            column(6, selectInput("search_issue", "Выпуск:", choices = "Все"))),
                   actionButton("search_btn", "Найти", class = "btn-primary w-100"),
                   hr(), uiOutput("search_sidebar_list")),
      mainPanel(uiOutput("search_results_cards"))
    )),
    tabPanel("📅 Архив", sidebarLayout(
      sidebarPanel(class = "sticky-sidebar",
                   selectInput("filter_year", "Год:", choices = NULL),
                   selectInput("filter_issue", "Выпуск:", choices = NULL),
                   hr(), uiOutput("archive_sidebar_list")),
      mainPanel(uiOutput("archive_cards"))
    )),
    
    
    # ── НОВАЯ ВКЛАДКА: LLM Ассистент (автономная) ─────────────────────────────
    tabPanel("🧠 LLM Агент",
             sidebarLayout(
               sidebarPanel(
                 textAreaInput("llm_query", "Ваш вопрос:", 
                               rows = 3, 
                               placeholder = "Например: Что известно о лессовых отложениях?"),
                 fluidRow(
                   column(6, selectInput("llm_year", "Год:", choices = "Все")),
                   column(6, selectInput("llm_issue", "Выпуск:", choices = "Все"))
                 ),
                 actionButton("llm_send", "Задать вопрос", 
                              class = "btn-primary w-100", icon = icon("paper-plane"))
               ),
               mainPanel(
                 div(style = "max-height: 75vh; overflow-y: auto; padding: 15px;",
                     uiOutput("llm_chat_messages"))
               )
             )
    ),
    tabPanel("🧬 CAG Синтез",
             sidebarLayout(
               sidebarPanel(
                 class = "sticky-sidebar",
                 h5("1. Глобальный поиск", class = "text-primary"),
                 # Объединенное поле поиска
                 div(style = "display: flex; gap: 5px;",
                     textInput("cag_query", NULL, placeholder = "Криолитозона, эрозия, Обь...", width = "100%"),
                     actionButton("cag_search_btn", icon("search"), class = "btn-primary")
                 ),
                 hr(),
                 h5("2. Настройка анализа", class = "text-primary"),
                 uiOutput("cag_token_counter"),
                 textAreaInput("cag_llm_query", "Вопрос к выбранным статьям:", 
                               rows = 5, 
                               placeholder = "Например: Сведи в таблицу данные по стоку наносов из всех выбранных статей..."),
                 actionButton("cag_cag_send", "Запустить CAG-анализ", 
                              class = "btn-success w-100", icon = icon("wand-magic-sparkles")),
                 hr(),
                 h5("История обсуждения", class = "text-muted"),
                 uiOutput("cag_chat_history_summary") # Краткий лог чата
               ),
               
               mainPanel(
                 # 1. Оборачиваем всё в "раскладушку"
                 tags$details(
                   open = TRUE, # Список будет открыт по умолчанию, пока ты его не свернешь
                   tags$summary(h4("🔍 Список найденных статей (нажми, чтобы скрыть)", style="display:inline; cursor:pointer;")),
                   
                   # Твой существующий вывод списка
                   uiOutput("cag_hybrid_results_ui") 
                 ),
                 
                 hr(),
                 
                 # 2. Область ответа ассистента всегда остается под списком
                 div(id = "cag_response_area",
                     uiOutput("cag_full_chat_display"))
               )
             )
    )
    
  )
)


# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  all_articles <- reactive({
    con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)
    raw <- dbGetQuery(con, "SELECT origin, first(text) as text FROM chunks GROUP BY origin")
    dbDisconnect(con)
    raw %>% mutate(
      art_id = paste0("art_", row_number()),
      title = str_extract(text, "(?<=Статья: ).+?(?=\\n|$)") %>% str_trim(),
      authors = str_extract(text, "(?<=Авторы: ).+?(?=\\n|$)") %>% str_trim(),
      summary = str_extract(text, "(?<=Краткое содержание: ).+?(?=\\n|$)") %>% str_trim(),
      source = str_extract(text, "(?<=Источник: ).+?(?=\\n|$)") %>% str_trim(),
      year = str_extract(source, "^\\d{4}"),
      issue = str_extract(source, "(?<=\\.)\\d+")
    ) %>% filter(!is.na(title))
  })
  
  observe({
    years <- c("Все", sort(unique(all_articles()$year), decreasing = TRUE))
    updateSelectInput(session, "filter_year", choices = years[-1])
    updateSelectInput(session, "search_year", choices = years)
  })
  
  observeEvent(input$search_year, {
    choices <- if(input$search_year == "Все") "Все" else c("Все", sort(unique(all_articles()$issue[all_articles()$year == input$search_year])))
    updateSelectInput(session, "search_issue", choices = choices)
  })
  
  observeEvent(input$filter_year, {
    updateSelectInput(session, "filter_issue", choices = sort(unique(all_articles()$issue[all_articles()$year == input$filter_year])))
  })
  
  # ── Поиск по ключу ─────────────────
  # ── Поиск по ключу ─────────────────
  search_data <- eventReactive(input$search_btn, {
    req(input$query)
    raw_q <- trimws(input$query)
    
    # Получаем нормальную форму для ИИ поиска
    lemma_q <- expand_query(raw_q) 
    
    if (input$search_type == "sem") {
      combined_q <- paste(raw_q, lemma_q)
      res <- vector_retrieve(combined_q, k = 50, input$search_year, input$search_issue)
      if (nrow(res) > 0) res <- rerank_results(raw_q, res)
    } else {
      # ВАЖНО: Сюда передаем raw_q, а не lemma_q!
      res <- keyword_retrieve(raw_q, input$search_year, input$search_issue)
    }
    
    if (is.null(res) || nrow(res) == 0) return(NULL)
    
    # Соединяем с базой статей
    res %>%
      left_join(all_articles() %>% select(origin, art_id, title, authors, summary, source), by = "origin") %>%
      filter(!is.na(title)) %>%
      { if(input$search_type == "sem") arrange(., desc(rerank_score)) else . } %>%
      head(15)
  })
  
  
  output$search_sidebar_list <- renderUI({
    data <- search_data()
    if (is.null(data)) return("Ничего не найдено")
    lapply(1:nrow(data), function(i) {
      row <- data[i, ]
      div(class = "sidebar-item",
          onclick = sprintf("var el = document.getElementById('search_%s'); el.scrollIntoView({behavior: 'smooth', block: 'center'}); el.classList.add('highlight-flash-row'); setTimeout(function(){ el.classList.remove('highlight-flash-row'); }, 2000);", row$art_id),
          span(row$title))
    })
  })
  
  output$search_results_cards <- renderUI({
    data <- search_data()
    if (is.null(data)) return(h4("Результатов нет."))
    
    # Оригинальное слово для подсветки в режиме ключевых слов
    raw_q <- trimws(input$query)
    
    lapply(1:nrow(data), function(i) {
      row <- data[i, ]
      md_file <- file.path(clean_llm_dir, paste0(row$source, "_clean_llm.md"))
      
      if (input$search_type == "sem") {
        # 1. СЕМАНТИЧЕСКИЙ ПОИСК (оставляем твою логику без изменений)
        full_chunk <- clean_chunk_preview(row$text)
        
        clean_anchor_text <- full_chunk %>%
          str_replace_all("(?i)(Статья|Авторы|Источник|Краткое содержание):.*", "") %>%
          str_replace_all("[\r\n\t]+", " ") %>%
          str_replace_all("\\s+", " ") %>%
          str_trim()
        
        target_text <- if (nchar(clean_anchor_text) > 40) {
          str_sub(clean_anchor_text, 15, 95)
        } else {
          str_sub(clean_anchor_text, 1, 80)
        }
        
        card_body <- div(class = "card-body",
                         h5(row$title, style = "color: #2c3e50; font-weight: bold;"),
                         p(class = "text-muted", style = "font-size: 0.85rem;", paste(row$authors, "—", row$source)),
                         p(style = "white-space: pre-wrap; font-size: 0.95rem; color: #0d6efd;", full_chunk),
                         div(class = "text-end", span(class = "badge bg-success", round(row$rerank_score, 3))))
      } else {
        # 2. ПОИСК ПО КЛЮЧУ (вот здесь заменяем логику якоря)
        # Разбиваем запрос на слова и берем самое длинное (чтобы JS его точно нашел в тексте)
        words <- unlist(strsplit(raw_q, "\\s+"))
        valid_words <- words[nchar(words) > 3] # игнорируем мелкие слова
        
        if (length(valid_words) > 0) {
          target_text <- valid_words[which.max(nchar(valid_words))]
        } else {
          target_text <- words[1]
        }
        
        card_body <- div(class = "card-body", 
                         h5(row$title), 
                         p(class = "text-muted", row$authors),
                         p(style = "color: #6c757d; font-size: 0.9rem;", 
                           tags$i("Найдено по ключевому слову")))
      }
      
      # Кодируем путь и якорь
      encoded <- paste0(URLencode(md_file, reserved = TRUE), "|||", URLencode(target_text, reserved = TRUE))
      
      div(class = "card", id = paste0("search_", row$art_id),
          onclick = sprintf("Shiny.setInputValue('open_article', '%s', {priority: 'event'})", encoded),
          card_body)
    })
  })
  
  # Архив (без изменений)
  archive_filtered <- reactive({
    req(input$filter_year, input$filter_issue)
    all_articles() %>% filter(year == input$filter_year, issue == input$filter_issue)
  })
  
  output$archive_sidebar_list <- renderUI({
    data <- archive_filtered()
    lapply(1:nrow(data), function(i) {
      row <- data[i, ]
      div(class = "sidebar-item",
          onclick = sprintf("var el = document.getElementById('arch_%s'); el.scrollIntoView({behavior: 'smooth', block: 'center'}); el.classList.add('highlight-flash-row'); setTimeout(function(){ el.classList.remove('highlight-flash-row'); }, 2000);", row$art_id),
          span(row$title))
    })
  })
  
  output$archive_cards <- renderUI({
    data <- archive_filtered()
    lapply(1:nrow(data), function(i) {
      row <- data[i, ]
      md_file <- file.path(clean_llm_dir, paste0(row$source, "_clean_llm.md"))
      encoded <- paste0(URLencode(md_file, reserved = TRUE), "|||", URLencode(row$title, reserved = TRUE))
      div(class = "card", id = paste0("arch_", row$art_id),
          onclick = sprintf("Shiny.setInputValue('open_article', '%s', {priority: 'event}')", encoded),
          div(class = "card-body", h5(row$title), p(class="text-muted", row$authors), div(row$summary)))
    })
  })
  
  #---------------------------------------------------------- Обычный поиск ------------------------------------------------------------------------------
  observeEvent(input$open_article, {
    req(input$open_article)
    parts <- strsplit(input$open_article, "|||", fixed = TRUE)[[1]]
    file_path <- URLdecode(parts[1])
    chunk_anchor <- if (length(parts) > 1) URLdecode(parts[2]) else ""
    
    if (file.exists(file_path)) {
      # Передаем не весь текст, а только корень для подсветки
      # Если это ключевой поиск, берем первые 6-7 символов
      js_search_str <- jsonlite::toJSON(chunk_anchor, auto_unbox = TRUE)
      
      showModal(modalDialog(
        title = basename(file_path),
        size = "xl",
        easyClose = TRUE,
        div(
          style = "max-height: 80vh; overflow-y: auto; padding: 20px;",
          div(class = "markdown-body", includeMarkdown(file_path)),
          
          tags$script(HTML(sprintf(
            '
            setTimeout(function() {
              $(".modal-body").each(function() {
                var container = this;
                var searchStr = %s;
                if (!searchStr || searchStr.length < 2) return;

                var markdownBody = $(container).find(".markdown-body");
                var html = markdownBody.html();
                
                // 1. Берем КОРЕНЬ слова (первые 6-7 символов)
                // Это позволит подсветить и "дендрохронология", и "дендро-хронология"
                var root = searchStr.substring(0, 7).toLowerCase();
                
                // 2. Регулярка, которая ищет корень, игнорируя возможные переносы (-) и спецсимволы внутри
                // [\\\\-\\\\s]* позволяет найти корень, даже если в нем тире или пробел
                var escapedRoot = root.split("").join("[\\\\-\\\\s]*");
                var regex = new RegExp("(" + escapedRoot + "[а-яёa-z]*)", "gi");
                
                // 3. Подсвечиваем все найденные совпадения
                var newHtml = html.replace(regex, \'<mark style="background-color: #ffeb3b; color: black; padding: 2px; border-radius: 3px;">$1</mark>\');
                markdownBody.html(newHtml);
                
                // 4. Скроллим к первому найденному
                var firstMark = markdownBody.find("mark").first();
                if (firstMark.length > 0) {
                  firstMark[0].scrollIntoView({ behavior: "smooth", block: "center" });
                }
              });
            }, 600);
            ',
            js_search_str
          )))
        )
      ))
    }
  })
  
  # ----------------------------------------------------------LLM АССИСТЕНТ--------------------------------------------------
  llm_history <- reactiveVal(list())
  
  # Рендер сообщений чата
  output$llm_chat_messages <- renderUI({
    history <- llm_history()
    if (length(history) == 0) {
      return(div(class = "text-center text-muted mt-5",
                 "Задайте вопрос — агент ответит на основе архива журнала «Геоморфология»"))
    }
    
    lapply(history, function(msg) {
      div(class = paste("llm-message", if (msg$role == "user") "user-msg" else "assistant-msg"),
          strong(if (msg$role == "user") "Вы" else "Ассистент:"),
          
          if (msg$role == "assistant") {
            tagList(
              markdown(msg$content %||% ""),
              
              # Блок источников — с защитой от ошибок
              if (!is.null(msg$sources) && length(msg$sources) > 0) {
                div(class = "mt-3 pt-2 border-top",
                    h6("Использованные источники:", style = "color: #666; font-size: 0.95rem;"),
                    tags$ul(style = "padding-left: 20px;",
                            lapply(seq_along(msg$sources), function(i) {
                              src <- msg$sources[[i]]
                              # Защита: если это не список/датафрейм, пропускаем
                              if (!is.list(src) && !is.data.frame(src)) return(NULL)
                              
                              title <- src$title %||% src[["title"]] %||% "Без названия"
                              encoded <- src$encoded %||% src[["encoded"]] %||% ""
                              
                              tags$li(
                                tags$a(href = "#", 
                                       onclick = sprintf("Shiny.setInputValue('open_article', '%s', {priority: 'event'})", encoded),
                                       title)
                              )
                            })
                    )
                )
              }
            )
          } else {
            p(msg$content, style = "margin: 0;")
          }
      )
    })
  })
  
  # Кнопка отправки
  observeEvent(input$llm_send, {
    req(input$llm_query)
    
    user_query <- trimws(input$llm_query)
    if (user_query == "") return()
    
    # Добавляем сообщение пользователя
    current <- llm_history()
    current <- append(current, list(list(role = "user", content = user_query)))
    llm_history(current)
    
    updateTextAreaInput(session, "llm_query", value = "")
    
    # Прогресс + логика
    withProgress(message = "Агент думает…", value = 0, {
      
      incProgress(0.3, detail = "Поиск релевантных чанков...")
      
      raw_results <- vector_retrieve(user_query, k = 40, 
                                     year_filter = input$llm_year %||% "Все", 
                                     issue_filter = input$llm_issue %||% "Все")
      
      sources_info <- NULL
      
      if (nrow(raw_results) > 0) {
        reranked <- rerank_results(user_query, raw_results) %>% head(8)
        
        sources_info <- reranked %>%
          left_join(all_articles() %>% select(origin, title, authors, source), by = "origin") %>%
          mutate(
            title = coalesce(title, str_extract(origin, "[^/]+$"), "Без названия"),
            clean_anchor = clean_chunk_preview(text, max_chars = 120),
            encoded = paste0(
              URLencode(file.path(clean_llm_dir, paste0(source, "_clean_llm.md")), reserved = TRUE),
              "|||",
              URLencode(clean_anchor, reserved = TRUE)
            )
          ) %>%
          select(title, encoded) %>%
          distinct(title, .keep_all = TRUE) %>%
          as.list() %>%                    # Превращаем в список списков (безопасно)
          purrr::transpose()
      }
      
      incProgress(0.7, detail = "Генерация ответа...")
      
      answer <- agent_respond(
        query       = user_query,
        history     = current,
        year_filter = input$llm_year %||% "Все",
        issue_filter = input$llm_issue %||% "Все",
        k = 8
      )
      
      incProgress(1, detail = "Готово")
    })
    
    # Добавляем ответ + источники
    assistant_msg <- list(
      role = "assistant",
      content = answer,
      sources = sources_info
    )
    
    current <- llm_history()
    current <- append(current, list(assistant_msg))
    llm_history(current)
  })
  
  # -------------CAG -----------------------------------------------------------------------------------------------
  hybrid_res <- reactiveVal(NULL)      # Результаты поиска статей
  cag_history <- reactiveVal(list())   # История чата в CAG
  
  # ===========================================================================
  # 1. ГИБРИДНЫЙ ПОИСК (Вкладка CAG)
  # ===========================================================================
  
  observeEvent(input$cag_search_btn, {
    req(input$cag_query)
    raw_query <- trimws(input$cag_query)
    
    withProgress(message = "Выполняем гибридный поиск...", value = 0, {
      
      # --- ШАГ 0: "УМНАЯ" ПРЕДОБРАБОТКА (pymorphy3) ---
      incProgress(0.1, detail = "Морфологический анализ...")
      
      # 1. Используем библиотеку для нормализации падежей
      # Теперь "дендрохронологией" превращается в "дендрохронология" интеллектуально
      clean_query <- expand_query(raw_query) 
      
      # 2. Дробление для борьбы с "битым" текстом (OCR)
      expanded_parts <- ""
      if(nchar(raw_query) > 10) {
        mid <- floor(nchar(raw_query) / 2)
        expanded_parts <- paste(substr(raw_query, 1, mid), substr(raw_query, mid + 1, nchar(raw_query)))
      }
      
      # Финальный микс для вектора: Оригинал + Леммы + Половинки
      search_query <- paste(raw_query, clean_query, expanded_parts)
      # -----------------------------------------------
      
      # А. Семантический поиск
      incProgress(0.3, detail = "ИИ-поиск и ранжирование...")
      sem_data <- vector_retrieve(search_query, k = 40)
      
      if(!is.null(sem_data) && nrow(sem_data) > 0) {
        # Реранкер оставляем по оригинальному запросу, чтобы не путать ИИ
        sem_data <- rerank_results(raw_query, sem_data)
      }
      
      # Б. Поиск по ключевым словам (Тут леммы от pymorphy дают максимум профита)
      incProgress(0.3, detail = "Поиск по точным совпадениям...")
      key_data <- keyword_retrieve(clean_query) 
      
      # В. Слияние результатов
      incProgress(0.2, detail = "Слияние потоков...")
      
      sem_ids <- if(!is.null(sem_data) && nrow(sem_data) > 0) unique(sem_data$origin) else character(0)
      key_ids <- if(!is.null(key_data) && nrow(key_data) > 0) unique(key_data$origin) else character(0)
      all_ids <- unique(c(sem_ids, key_ids))
      
      if(length(all_ids) == 0) {
        hybrid_res(NULL)
        showNotification("Статьи не найдены", type = "warning")
        return()
      }
      
      final_df <- all_articles() %>%
        filter(origin %in% all_ids) %>%
        mutate(
          is_semantic = origin %in% sem_ids,
          is_keyword = origin %in% key_ids
        ) %>%
        arrange(desc(is_semantic & is_keyword), desc(is_semantic))
      
      hybrid_res(final_df)
    })
  })
  
  # 2. РЕНДЕР ИНТЕРФЕЙСА РЕЗУЛЬТАТОВ (Исправлено: убран hash_string)
  output$cag_hybrid_results_ui <- renderUI({
    data <- hybrid_res()
    if (is.null(data) || nrow(data) == 0) return(div(class="text-center p-5 text-muted", "Введите запрос..."))
    
    tagList(
      div(class = "list-group shadow-sm",
          lapply(1:nrow(data), function(i) {
            item <- data[i, ]
            safe_id <- paste0("check_", gsub("[^a-zA-Z0-9]", "_", item$origin))
            
            div(class = "list-group-item list-group-item-action d-flex align-items-center",
                style = "border-left: 4px solid #2c3e50; padding: 12px;",
                checkboxInput(safe_id, label = NULL, value = FALSE, width = "40px"),
                div(style = "flex-grow: 1;",
                    div(class = "d-flex justify-content-between align-items-center",
                        strong(item$title, style="font-size: 1.1rem;"),
                        div(
                          # Смысл теперь синий (Primary)
                          if(item$is_semantic) span(class="badge bg-primary text-white me-1", "Смысл") else NULL,
                          # Ключ теперь ЗЕЛЕНЫЙ с БЕЛЫМ текстом (как ты просил)
                          if(item$is_keyword)  span(class="badge bg-success text-white", "Ключ") else NULL
                        )
                    ),
                    div(class="text-muted", style="font-size: 0.85rem;",
                        paste(item$authors, "|", item$year))
                )
            )
          })
      )
    )
  })
  
  # 3. СБОР ВЫБРАННЫХ ID (Исправлено: логика формирования ID совпадает с UI)
  selected_origins <- reactive({
    data <- hybrid_res()
    if (is.null(data)) return(character(0))
    
    results <- c()
    for(i in 1:nrow(data)) {
      origin_id <- data$origin[i]
      # Тот же самый gsub, что и в UI
      safe_id <- paste0("check_", gsub("[^a-zA-Z0-9]", "_", origin_id))
      
      if (!is.null(input[[safe_id]]) && input[[safe_id]]) {
        results <- c(results, origin_id)
      }
    }
    return(results)
  })
  
  # ===========================================================================
  # 4. СЧЕТЧИК ТОКЕНОВ (Исправлен для DuckDB)
  # ===========================================================================
  
  output$cag_token_counter <- renderUI({
    origins <- selected_origins()
    if (length(origins) == 0) return(div(class="alert alert-secondary py-2", "Статьи не выбраны"))
    
    con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)
    # Прямая сборка строки для IN во избежание ошибок VARCHAR[]
    safe_list <- paste0("'", gsub("'", "''", origins), "'", collapse = ", ")
    query <- sprintf("SELECT SUM(LENGTH(text)) as total_chars FROM chunks WHERE origin IN (%s)", safe_list)
    
    res <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    chars <- res$total_chars
    if (is.na(chars)) chars <- 0
    
    tokens <- round(chars / 3.5) # Эмпирический коэффициент для кириллицы
    
    badge_class <- if(tokens < 40000) "bg-success" else if(tokens < 90000) "bg-warning" else "bg-danger"
    
    div(class=paste("badge w-100 p-2 mb-2", badge_class),
        style="font-size: 0.9rem; white-space: normal;",
        paste("Объем контекста:", format(tokens, big.mark=" "), "токенов"))
  })
  
  # ===========================================================================
  # 5. CAG ГЕНЕРАЦИЯ (Сбор всех чанков и ответ)
  # ===========================================================================
  
  observeEvent(input$cag_cag_send, {
    origins <- selected_origins()
    req(input$cag_llm_query, length(origins) > 0)
    
    user_q <- trimws(input$cag_llm_query)
    # Добавляем в историю вопрос пользователя
    cag_history(append(cag_history(), list(list(role = "user", content = user_q))))
    
    withProgress(message = "Синтезирую ответ по выбранным статьям...", value = 0.2, {
      
      # 1. Загрузка всех текстов чанков для выбранных статей
      con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)
      safe_list <- paste0("'", gsub("'", "''", origins), "'", collapse = ", ")
      full_texts <- dbGetQuery(con, sprintf("SELECT origin, text FROM chunks WHERE origin IN (%s)", safe_list))
      dbDisconnect(con)
      
      incProgress(0.3, detail = "Формирую базу знаний...")
      
      # 2. Группировка текстов по статьям (XML-разметка для LLM)
      context_bundle <- full_texts %>%
        left_join(all_articles() %>% select(origin, title, authors, year), by = "origin") %>%
        group_by(origin, title, authors, year) %>%
        summarise(all_text = paste(text, collapse = "\n\n"), .groups = "drop") %>%
        mutate(formatted = sprintf("<article id='%s' title='%s' year='%s'>\n%s\n</article>", 
                                   origin, title, year, all_text)) %>%
        pull(formatted) %>%
        paste(collapse = "\n\n")
      
      # 3. Системный промпт для CAG
      system_prompt <- paste0(
        "Ты — профессиональный научный аналитик. Твоя задача — провести глубокий анализ ПОЛНЫХ текстов предоставленных статей.\n",
        "ПРАВИЛА:\n",
        "1. Используй ТОЛЬКО предоставленный текст.\n",
        "2. Цитируй конкретные данные, методы и выводы.\n",
        "3. Если статьи противоречат друг другу, укажи на это.\n\n",
        "КОНТЕКСТ СТАТЕЙ:\n", context_bundle
      )
      
      incProgress(0.4, detail = "Нейросеть изучает данные...")
      
      # 4. Вызов API
      answer <- or_chat(
        prompt = paste(system_prompt, "\n\nВОПРОС ПОЛЬЗОВАТЕЛЯ:", user_q),
        keys = keys,
        model = "stepfun/step-3.5-flash:free"
      )
      
      # Сохраняем ответ в историю
      cag_history(append(cag_history(), list(list(role = "assistant", content = answer))))
    })
    
    # Очищаем поле ввода
    updateTextAreaInput(session, "cag_llm_query", value = "")
  })
  
  # ===========================================================================
  # 6. ВЫВОД ЧАТА CAG
  # ===========================================================================
  
  output$cag_full_chat_display <- renderUI({
    history <- cag_history()
    if (length(history) == 0) return(NULL)
    
    lapply(rev(history), function(msg) { # Выводим последние сообщения сверху
      if (msg$role == "assistant") {
        div(class = "card mb-4 shadow border-0",
            div(class = "card-header bg-dark text-white d-flex justify-content-between",
                span(icon("microchip"), " Аналитический синтез (Step-3.5-Flash)"),
                span(class="badge bg-success", "CAG Mode")),
            div(class = "card-body bg-light", 
                style = "font-size: 1.05rem; line-height: 1.6;",
                markdown(msg$content)))
      } else {
        div(class = "alert alert-primary shadow-sm mb-2",
            style = "border-left: 5px solid #0d6efd;",
            icon("user-circle"), strong(" Ваш запрос: "), msg$content)
      }
    })
  })
}



shinyApp(ui = ui, server = server)
