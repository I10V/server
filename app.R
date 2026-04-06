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
rerank_results <- function(query, candidates_df) {
  if (nrow(candidates_df) == 0) return(candidates_df)
  pairs <- lapply(candidates_df$text, function(txt) list(query, txt))
  scores <- reranker_model$predict(pairs)
  candidates_df$rerank_score <- as.numeric(scores)
  candidates_df[order(-candidates_df$rerank_score), ]
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
    ))
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
  
  search_data <- eventReactive(input$search_btn, {
    req(input$query)
    if (input$search_type == "sem") {
      res <- vector_retrieve(input$query, k = 40, input$search_year, input$search_issue)
      if (nrow(res) > 0) res <- rerank_results(input$query, res)
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
  
  # ── МОДАЛКА — ТОЧНО ТВОЙ ОРИГИНАЛЬНЫЙ РАБОЧИЙ КОД (sprintf) ─────────────
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
            setTimeout(function() {
              var container = document.querySelector(".modal-body");
              var searchStr = %s;
              if (!searchStr || searchStr.length < 4) return;
              var cleanSearch = searchStr.replace(/\\s+/g, " ").trim().toLowerCase();
             
              var elements = container.querySelectorAll("p, li, td, h1, h2, h3, h4, span");
              var target = null;
              for (var i = 0; i < elements.length; i++) {
                var elText = elements[i].textContent.replace(/\\s+/g, " ").toLowerCase();
                if (elText.includes(cleanSearch)) {
                  target = elements[i];
                  break;
                }
              }
              if (!target && cleanSearch.length > 25) {
                var shortSearch = cleanSearch.substring(0, 25);
                for (var i = 0; i < elements.length; i++) {
                  if (elements[i].textContent.toLowerCase().includes(shortSearch)) {
                    target = elements[i];
                    break;
                  }
                }
              }
              if (target) {
                target.scrollIntoView({ behavior: "smooth", block: "center" });
                target.style.backgroundColor = "#fff3cd";
                target.style.transition = "background-color 0.5s ease";
                target.style.borderRadius = "4px";
               
                var count = 0;
                var blink = setInterval(function() {
                  target.style.backgroundColor = (count %% 2 === 0) ? "transparent" : "#fff3cd";
                  if (++count >= 6) {
                    clearInterval(blink);
                    target.style.backgroundColor = "#fff3cd";
                  }
                }, 300);
               
                setTimeout(function() { target.style.backgroundColor = "transparent"; }, 6000);
              } else {
                console.log("Текст не найден. Искали:", cleanSearch);
              }
            }, 1000);
            ',
            js_search_str
          )))
        )
      ))
    } else {
      showNotification("Файл статьи не найден на диске", type = "error")
    }
  })
}
shinyApp(ui = ui, server = server)