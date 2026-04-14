# Цифровой архив журнала «Геоморфология»

[![Shiny](https://img.shields.io/badge/Shiny-0.0.0.9000-blue?logo=r)](https://shiny.posit.co/)
[![R](https://img.shields.io/badge/R-4.3+-blue?logo=r)](https://www.r-project.org/)
[![Ollama](https://img.shields.io/badge/Ollama-local-green?logo=ollama)](https://ollama.com/)
[![DuckDB](https://img.shields.io/badge/DuckDB-vector_db-orange)](https://duckdb.org/)

**Интеллектуальная поисковая система + LLM-агент + CAG-синтез** для полного цифрового архива журнала «Геоморфология».

Приложение позволяет:
- Искать статьи **по смыслу** (векторный поиск + реранкер)
- Искать **по ключевым словам** (с морфологической нормализацией)
- Задавать вопросы **LLM-агенту**, который отвечает строго по архиву
- Проводить **CAG-анализ** (Contextual Augmented Generation) — выбирать статьи и получать глубокий синтез

---

## ✨ Основные возможности

### 🔍 Поиск
- **Смысловой поиск** — векторные эмбеддинги (`nomic-embed-text` через Ollama) + CrossEncoder-ранжирование (`BAAI/bge-reranker-v2-m3`)
- **Поиск по ключевым словам** — морфологическая нормализация через `pymorphy3`
- Фильтры по году и выпуску
- Красивые карточки с подсветкой найденных фрагментов

### 🧠 LLM-Агент (вкладка «LLM Агент»)
- Полноценный чат с **RAG** (Retrieval-Augmented Generation)
- Поддержка истории диалога (multi-turn)
- Ответы только на основе архива журнала
- Автоматическое прикрепление источников с возможностью открыть статью

### 🧬 CAG Синтез (вкладка «CAG Синтез»)
- Гибридный поиск (семантический + ключевой)
- Выбор нужных статей чекбоксами
- Автоматический подсчёт токенов контекста
- Глубокий анализ **полных текстов** выбранных статей через Step-3.5-Flash

### 📂 Архив
- Просмотр всех статей по годам и выпускам
- Быстрый переход к любой статье

---

## 🛠️ Требования и установка

### 1. Системные требования
- **R** ≥ 4.3
- **Python** 3.10–3.12 (через `reticulate`)
- **Ollama** (локально)
- Windows (код оптимизирован под Windows-пути)

### 2. Установка

```bash
# 1. Клонируйте репозиторий
git clone https://github.com/ВАШ_НИК/geomorf-archive-shiny.git
cd geomorf-archive-shiny

# 2. Установите R-пакеты
Rscript -e "install.packages(c('shiny', 'tidyverse', 'stringr', 'duckdb', 'DBI', 'ellmer', 'ragnar', 'markdown', 'htmltools', 'jsonlite', 'readxl', 'bslib', 'purrr'))"
