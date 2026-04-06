# =============================================================================
# ПРИНУДИТЕЛЬНАЯ НАСТРОЙКА PYTHON (Должна быть в самом верху!)
# =============================================================================
# 1. Отключаем авто-управление пакетами
Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")

# 2. Указываем путь к конкретному исполняемому файлу
Sys.setenv(RETICULATE_PYTHON = "C:/Users/user/Documents/.virtualenvs/r-reticulate/Scripts/python.exe")

# 3. Инициализируем мост R-Python
library(reticulate)

# 4. Блокируем выбор окружения (required = TRUE — это ключ к успеху)
use_virtualenv("r-reticulate", required = TRUE)

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

# Запуск Ollama
shell("start ollama serve", wait = FALSE)
Sys.sleep(2)
# ── Инициализация ───────────────────────────────────────────────────────────
if (!exists("reranker_model")) {
  cross_encoder <- import("sentence_transformers")$CrossEncoder
  reranker_model <- cross_encoder("BAAI/bge-reranker-v2-m3", device = "cpu")
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
  sql <- "SELECT origin, text, 1.0 as similarity FROM chunks WHERE text ILIKE ?"
  params <- list(paste0("%", query, "%"))
  if (year_filter != "Все") { sql <- paste(sql, "AND regexp_extract(origin, '^\\d{4}') = ?"); params <- c(params, year_filter) }
  if (issue_filter != "Все") { sql <- paste(sql, "AND regexp_extract(origin, '\\.(\\d+)') = ?"); params <- c(params, issue_filter) }
  res <- dbGetQuery(con, sql, params)
  dbDisconnect(con)
  res
}
rerank_results <- function(query, docs) {
  # Проверка: если документов нет, возвращаем пустой результат
  if (is.null(docs) || nrow(docs) == 0) return(docs)
  
  # Создаем пары для нейросети (запрос + текст каждого чанка)
  # Используем docs (то, что пришло в функцию), а не candidates_df
  pairs <- lapply(docs$text, function(txt) list(query, txt))
  
  # Вызываем модель (через .GlobalEnv для надежности)
  scores <- .GlobalEnv$reranker_model$predict(pairs)
  
  # Записываем баллы и сортируем
  docs$rerank_score <- as.numeric(scores)
  docs[order(-docs$rerank_score), ]
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
  
# ── ИСПРАВЛЕННЫЙ КУСОК (замени полностью старый search_data) ─────────────────
search_data <- eventReactive(input$search_btn, {
  req(input$query)
  
  if (input$search_type == "sem") {
    withProgress(message = "Поиск в архиве...", value = 0, {
      incProgress(0.3, detail = "Векторный поиск чанков...")
      res <- vector_retrieve(input$query, k = 40, input$search_year, input$search_issue)
      
      incProgress(0.7, detail = "Реранкинг результатов...")
      if (nrow(res) > 0) {
        res <- rerank_results(input$query, res)
      }
      
      incProgress(1, detail = "Готово")
    })
  } else {
    res <- keyword_retrieve(input$query, input$search_year, input$search_issue)
  }
  
  if (nrow(res) == 0) return(NULL)
  
  data <- res %>%
    left_join(all_articles() %>% select(origin, art_id, title, authors, summary, source), by = "origin") %>%
    filter(!is.na(title))
  
  if (input$search_type == "sem") {
    data <- data %>% arrange(desc(rerank_score)) %>% slice(1:min(15, n()))
  } else {
    data <- data %>%
      group_by(origin) %>%
      slice_max(order_by = similarity, n = 1, with_ties = FALSE) %>%
      ungroup()
  }
  data
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
    lapply(1:nrow(data), function(i) {
      row <- data[i, ]
      md_file <- file.path(clean_llm_dir, paste0(row$source, "_clean_llm.md"))
      
      if (input$search_type == "sem") {
        full_chunk <- clean_chunk_preview(row$text)
        clean_anchor_text <- full_chunk %>%
          str_replace_all("(?i)(Статья|Авторы|Источник|Краткое содержание):.*", "") %>%
          str_replace_all("[\r\n\t]+", " ") %>%
          str_replace_all("\\s+", " ") %>%
          str_trim()
        chunk_anchor <- if (nchar(clean_anchor_text) > 40) {
          str_sub(clean_anchor_text, 15, 95)
        } else {
          str_sub(clean_anchor_text, 1, 80)
        }
        target_text <- chunk_anchor
        
        card_body <- div(class = "card-body",
                         h5(row$title, style = "color: #2c3e50; font-weight: bold;"),
                         p(class = "text-muted", style = "font-size: 0.85rem;", paste(row$authors, "—", row$source)),
                         p(style = "white-space: pre-wrap; font-size: 0.95rem; color: #0d6efd;", full_chunk),
                         div(class = "text-end", span(class = "badge bg-success", round(row$rerank_score, 3))))
      } else {
        target_text <- input$query
        card_body <- div(class = "card-body", h5(row$title), p(class = "text-muted", row$authors))
      }
      
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
  # ── ИСПРАВЛЕННЫЙ КУСОК (замени полностью старый observeEvent для open_article) ─────
  # Самая надёжная версия на сегодня:
  # • Для «Точное слово» — глобальная подсветка ВСЕХ вхождений слова через <mark>
  # • Скролл точно к первому найденному слову
  # • Никакого закрашивания всей страницы
  # • Работает даже если текст в сложных тегах
  
  observeEvent(input$open_article, {
    req(input$open_article)
    parts <- strsplit(input$open_article, "|||", fixed = TRUE)[[1]]
    file_path <- URLdecode(parts[1])
    chunk_anchor <- if (length(parts) > 1) URLdecode(parts[2]) else ""
    
    if (file.exists(file_path)) {
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
          console.log("=== MODAL JS ЗАПУЩЕН (финальная версия) ===");
          setTimeout(function() {
            $(".modal-body").each(function() {
              var container = this;
              var searchStr = %s;
              
              if (!searchStr || searchStr.length < 2) return;
              
              var cleanSearch = searchStr.replace(/\\s+/g, " ").trim().toLowerCase();
              var isKeywordSearch = cleanSearch.length < 60;
              
              console.log("Запрос:", searchStr, "| Тип:", isKeywordSearch ? "ТОЧНОЕ СЛОВО" : "СЕМАНТИКА");
              
              if (isKeywordSearch) {
                // ── ГЛОБАЛЬНАЯ ПОДСВЕТКА ДЛЯ ОБЫЧНОГО ПОИСКА ──
                var markdownBody = $(container).find(".markdown-body");
                if (markdownBody.length === 0) return;
                
                var html = markdownBody.html();
                var escaped = cleanSearch.replace(/[.*+?^${}()|[\\]\\\\]/g, "\\\\$&");
                var regex = new RegExp(escaped, "gi");
                
                var newHtml = html.replace(regex, \'<mark style="background-color: #ffeb3b; padding: 2px 4px; border-radius: 3px; box-shadow: 0 0 8px rgba(255,235,59,0.9);">$&</mark>\');
                
                markdownBody.html(newHtml);
                
                var firstMark = markdownBody.find("mark").first();
                if (firstMark.length > 0) {
                  firstMark[0].scrollIntoView({ behavior: "smooth", block: "center" });
                  console.log("✅ Слово найдено и подсвечено, скролл выполнен");
                } else {
                  console.log("❌ Слово НЕ НАЙДЕНО в статье");
                }
              } else {
                // ── СТАРАЯ ЛОГИКА ДЛЯ СЕМАНТИЧЕСКОГО ПОИСКА (не трогаем) ──
                var elements = $(container).find(".markdown-body p, .markdown-body li, .markdown-body td, .markdown-body h1, .markdown-body h2, .markdown-body h3, .markdown-body h4");
                var target = null;
                elements.each(function() {
                  var elText = $(this).text().replace(/\\s+/g, " ").toLowerCase();
                  if (elText.includes(cleanSearch)) {
                    target = this;
                    return false;
                  }
                });
                if (!target && cleanSearch.length > 25) {
                  var shortSearch = cleanSearch.substring(0, 25);
                  elements.each(function() {
                    if ($(this).text().toLowerCase().includes(shortSearch)) {
                      target = this;
                      return false;
                    }
                  });
                }
                if (target) {
                  target.scrollIntoView({ behavior: "smooth", block: "center" });
                  $(target).css({backgroundColor: "#fff3cd", borderRadius: "4px"});
                  var count = 0;
                  var blink = setInterval(function() {
                    $(target).css("background-color", (count %% 2 === 0) ? "transparent" : "#fff3cd");
                    if (++count >= 6) {
                      clearInterval(blink);
                      $(target).css("background-color", "#fff3cd");
                    }
                  }, 300);
                  setTimeout(function() { $(target).css("background-color", "transparent"); }, 6000);
                }
              }
            });
          }, 1500);
          ',
            js_search_str
          )))
        )
      ))
    } else {
      showNotification("Файл статьи не найден на диске", type = "error")
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
  
}


shinyApp(ui = ui, server = server)