#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(stopwords)
library(udpipe)
library(DT)
library(bslib)
library(showtext)
library(tidyr)
library(wordcloud2)
library(plotly)
library(scales)  


# Шаг 1. Подключение шрифтов 
font_add_google("Inter", "Inter")
showtext_auto()
showtext::showtext_opts(dpi = 96)


# Шаг 2. Модель udpipe
model_file <- list.files(pattern = "russian-.*\\.udpipe")
if (length(model_file) == 0) {
  model_file <- udpipe_download_model(language = "russian")$file_model
}
ud_model_ru <- udpipe_load_model(model_file)
ru_stop <- stopwords(language = "ru")


# Шаг 3. Функции для анализа 
annotate_text <- function(txt, model = ud_model_ru) {
  if (is.null(txt) || nchar(trimws(txt)) == 0) {
    return(NULL)
  }
  ann <- udpipe_annotate(model, x = txt) |> as.data.frame()
  if (nrow(ann) == 0) return(NULL)
  
  ann |>
    mutate(
      token = tolower(token),
      lemma = tolower(lemma)
    ) |>
    select(sentence_id, token, lemma, upos) |>
    filter(!is.na(lemma), lemma != "")
}

word_freq <- function(ann, top_n = 100, min_freq = 1) {
  if (is.null(ann) || nrow(ann) == 0) return(NULL)
  ann |>
    filter(
      !lemma %in% ru_stop,
      str_detect(lemma, "[а-яё]"),
      nchar(lemma) > 2
    ) |>
    count(lemma, sort = TRUE) |>
    rename(word = lemma) |>
    filter(n >= min_freq) |>
    slice_head(n = top_n)
}

get_ngrams <- function(ann, top_n = 20) {
  if (is.null(ann) || nrow(ann) == 0) return(NULL)
  
  base <- ann |>
    group_by(sentence_id) |>
    mutate(
      token_1 = lead(token, 1),
      lemma_1 = lead(lemma, 1)
    ) |>
    ungroup()
  
  base |>
    filter(!is.na(token_1), !is.na(lemma_1)) |>
    filter(
      !lemma %in% ru_stop & !lemma_1 %in% ru_stop,
      str_detect(lemma, "[а-яё]") & str_detect(lemma_1, "[а-яё]"),
      nchar(lemma) > 2 & nchar(lemma_1) > 2
    ) |>
    unite("surface", c(token, token_1), sep = " ", remove = TRUE) |>
    unite("lemma_key", c(lemma, lemma_1), sep = " ", remove = TRUE) |>
    count(lemma_key, surface) |>
    group_by(lemma_key) |>
    arrange(desc(n), .by_group = TRUE) |>
    summarise(
      ngram = first(surface),
      n = sum(n),
      .groups = "drop"
    ) |>
    arrange(desc(n)) |>
    slice_head(n = top_n)
}

make_wordcloud <- function(ann, top_n = 100, min_freq = 1) {
  df <- word_freq(ann, top_n = top_n, min_freq = min_freq)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  names(df) <- c("word", "freq")
  
  max_freq <- max(df$freq)
  colors <- scales::col_numeric(
    palette = c("#AFA449", "#655D1D"),
    domain = c(1, max_freq)
  )(df$freq)
  
  wordcloud2(df, 
             size = 0.7, 
             color = colors,
             backgroundColor = "#D6D1BC",
             shuffle = FALSE,
             fontFamily = "Inter")
}

make_ngram_plot <- function(ann, top_n = 20) {
  df <- get_ngrams(ann, top_n = top_n)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  p <- df |>
    ggplot(aes(reorder(ngram, n), n)) +
    geom_col(fill = "#9C0D0F") +
    coord_flip() +
    labs(x = NULL, y = "частота", title = "Топ биграмм") +
    theme_minimal(base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = "#D6D1BC", color = NA),
      panel.background = element_rect(fill = "#D6D1BC", color = NA),
      panel.grid = element_blank()
    )
  
  ggplotly(p)
}


# Шаг 4. Анализ TF 
analyze_tf <- function(raw_text, top_n = 15) {
  if (is.null(raw_text) || nchar(trimws(raw_text)) == 0) {
    return(
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "Введите текст для визуализации частоты слов.") + 
        theme_void()
    )
  }
  
  text_df <- tibble(text = raw_text)
  stopwords <- stopwords(language = "ru")
  
  parsed_text <- udpipe_annotate(ud_model_ru, x = text_df$text) |> as.data.frame()
  if (nrow(parsed_text) > 0) {
    lemmatized_string <- paste(parsed_text$lemma[!is.na(parsed_text$lemma)], collapse = " ")
    text_df <- tibble(text = lemmatized_string)
  }
  
  tf_df <- text_df |> 
    unnest_tokens(word, text) |> 
    filter(!str_detect(word, "^[0-9]+$")) |> 
    filter(!word %in% stopwords) |> 
    count(word, sort = TRUE) |> 
    mutate(tf = n / sum(n)) |> 
    slice_max(n, n = top_n, with_ties = FALSE)
  
  if (nrow(tf_df) == 0) {
    return(ggplot() + annotate("text", x = 1, y = 1, label = "Недостаточно слов для анализа.") + theme_void())
  }
  
  ggplot(tf_df, aes(x = tf, y = fct_reorder(word, tf))) +
    geom_col(fill = "#6B7F64", alpha = 0.9) +   
    geom_text(aes(label = n), 
              hjust = -0.2, 
              size = 4,  
              color = "#3D402F", 
              family = "Inter") +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +  
    theme(
      text = element_text(family = "Inter"),
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "plain", 
                                 size = 12,
                                 family = "Inter"),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 30),
      plot.background = element_rect(fill = "#D6D1BC", color = NA),
      panel.background = element_rect(fill = "#D6D1BC", color = NA)
    )
}

get_tf_table <- function(raw_text, top_n = 15) {
  if (is.null(raw_text) || nchar(trimws(raw_text)) == 0) {
    return(data.frame(Слово = "Нет данных", Частота = 0, TF = 0))
  }
  
  text_df <- tibble(text = raw_text)
  stopwords <- stopwords(language = "ru")
  
  parsed_text <- udpipe_annotate(ud_model_ru, x = text_df$text) |> as.data.frame()
  if (nrow(parsed_text) > 0) {
    lemmatized_string <- paste(parsed_text$lemma[!is.na(parsed_text$lemma)], collapse = " ")
    text_df <- tibble(text = lemmatized_string)
  }
  
  tf_df <- text_df |> 
    unnest_tokens(word, text) |> 
    filter(!str_detect(word, "^[0-9]+$")) |> 
    filter(!word %in% stopwords) |> 
    count(word, sort = TRUE) |> 
    mutate(tf = n / sum(n)) |> 
    slice_max(n, n = top_n, with_ties = FALSE)
  
  if (nrow(tf_df) == 0) {
    return(data.frame(Слово = "Недостаточно слов", Частота = 0, TF = 0))
  }
  tf_df |> 
    select(Слово = word, Частота = n, `Отн. частота (TF)` = tf)
}

# Шаг 5. Функции для анализа частей речи
get_pos_freq <- function(ann, top_n = 10) {
  if (is.null(ann) || nrow(ann) == 0) return(NULL)
  
  pos_labels <- c(
    "NOUN" = "Существительные",
    "VERB" = "Глаголы",
    "ADJ"  = "Прилагательные"
  )
  
  ann |>
    filter(upos %in% names(pos_labels)) |>
    mutate(part_of_speech = pos_labels[upos]) |>
    filter(!is.na(lemma), lemma != "", str_detect(lemma, "[а-яё]")) |>
    count(part_of_speech, lemma, sort = TRUE) |>
    group_by(part_of_speech) |>
    slice_max(n, n = top_n, with_ties = FALSE) |>
    ungroup() |>
    rename(word = lemma, freq = n)
}

plot_pos_freq <- function(ann, top_n = 10) {
  df <- get_pos_freq(ann, top_n)
  if (is.null(df) || nrow(df) == 0) {
    return(
      ggplot() + 
        annotate("text", x = 1, y = 1, 
                 label = "Недостаточно данных для анализа частей речи.") + 
        theme_void()
    )
  }
  
  df <- df |>
    group_by(part_of_speech) |>
    mutate(word = reorder_within(word, freq, part_of_speech)) |>
    ungroup()
  
  ggplot(df, aes(x = freq, y = word)) +
    geom_col(fill = "#AFA449", alpha = 0.9) +
    geom_text(aes(label = freq), 
              hjust = -0.2, 
              size = 4,
              color = "#3D402F",
              family = "Inter") +
    scale_y_reordered() +
    facet_wrap(~ part_of_speech, scales = "free_y", ncol = 3) +
    labs(
      x = "Частота",
      y = NULL,
      title = "Самые частотные слова по частям речи",
      subtitle = "Лемматизированный анализ (топ слов в каждой категории)"
    ) +
    theme_minimal(base_family = "Inter") +
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
      plot.subtitle = element_text(color = "gray40", size = 13, margin = margin(b = 15)),
      strip.text = element_text(face = "bold", size = 14, color = "#655D1D"),
      strip.background = element_rect(fill = "#D6D1BC", color = "#AFA449", size = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12, family = "Inter"),
      axis.text.x = element_text(size = 11, family = "Inter"),
      panel.spacing = unit(1.5, "lines"),
      plot.background = element_rect(fill = "#D6D1BC", color = NA),
      panel.background = element_rect(fill = "#D6D1BC", color = NA)
    )
}


# Шаг 6. Кастомная тема bslib
theme_custom <- bs_theme(
  bootswatch = "minty",
  primary = "#AFA449",
  bg = "#D6D1BC",
  fg = "#770205",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale = 0.85  
)

# Шаг 7. UI
ui <- page_sidebar(
  title = tags$div(
    "Анализатор текста",
    style = "font-family: 'Inter', sans-serif; 
             font-size: 3.2rem; 
             font-weight: 700; 
             text-transform: uppercase; 
             color: #D6D1BC; 
             letter-spacing: 0.02em; 
             margin-bottom: 0.2rem; 
             line-height: 1.1;"
  ),
  
  # Информация о приложении и о нас
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-top: 0.2rem; margin-bottom: 0.8rem; flex-wrap: wrap; gap: 10px;",
    
    # Название команды
    tags$div(
      "🍓 Клубничная команда: Авдеева Полина, Войтова Ксения, Кретов Владимир",
      style = "font-family: 'Inter', sans-serif;
               font-size: 1.4rem;
               color: #770205;
               flex: 1;"
    ),
    
    # Ссылка на GitHub
    tags$a(
      href = "https://github.com/polina03avdeevaa/App_R",
      target = "_blank",
      style = "display: inline-flex; align-items: center; gap: 8px; 
               background-color: #770205; color: #D6D1BC; 
               padding: 8px 16px; border-radius: 20px; 
               text-decoration: none; font-family: 'Inter', sans-serif;
               font-size: 1.1rem; font-weight: 500;
               transition: all 0.3s ease;
               border: 2px solid #770205;",
      onmouseover = "this.style.backgroundColor='#9C0D0F'; this.style.borderColor='#9C0D0F';",
      onmouseout = "this.style.backgroundColor='#770205'; this.style.borderColor='#770205';",
      tags$img(
        src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png",
        style = "height: 24px; width: 24px; filter: brightness(0) invert(1);"
      ),
      "GitHub"
    )
  ),

  
  
  theme = theme_custom,
  sidebar = sidebar(
    textAreaInput("user_text", "Вставьте ваш текст сюда:",
                  placeholder = "Ваш текст здесь",
                  rows = 6),
    helpText("Текст должен содержать не менее 10 слов"),
    sliderInput("top_words", "Количество слов на графиках:",
                min = 5, max = 15, value = 5, step = 1),
    actionButton("predict_btn", "🍨 Проанализировать текст", 
                 class = "btn-primary"), 
    # Короткое описание приложения
    tags$div(
      style = "background-color: rgba(175, 164, 73, 0.12); 
             border-left: 3px solid #AFA449; 
             padding: 8px 16px; 
             margin-bottom: 14px; 
             border-radius: 6px;
             font-family: 'Inter', sans-serif;",
      
      tags$p(
        style = "margin: 0; font-size: 0.95rem; line-height: 1.5; color: #3D402F;",
        tags$span(style = "font-weight: 700; color: #655D1D;", "☁️ Инструмент"),
        " для анализа русскоязычных текстов: подсчёт статистики, ",
        "частотность слов, облако, биграммы и части речи. ",
        "Использует", 
        tags$a(href = "https://ufal.mff.cuni.cz/udpipe", target = "_blank", 
               style = "color: #770205; font-weight: 500; text-decoration: underline;",
               "UDPipe"),
        " для лемматизации."
      )
    ),
  ),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap');
      
      body { 
        background-color: #D6D1BC !important;
        font-size: 16px !important;
      }
      
      .help-block {
        margin-top: 0.05rem !important;
        margin-bottom: 0.05rem !important;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
        font-size: 0.9rem !important;
      }
      
      .btn-primary,
      .dataTables_wrapper .dt-buttons .btn,
      .dataTables_wrapper .dt-buttons .btn-default {
        background-color: #AFA449 !important;
        border-color: #8f8838 !important;
        color: #655D1D !important;
        font-size: 1rem !important;
        padding: 8px 16px !important;
      }
      
      .btn-primary:hover,
      .dataTables_wrapper .dt-buttons .btn:hover,
      .dataTables_wrapper .dt-buttons .btn-default:hover {
        background-color: #9c9239 !important;
        border-color: #7a702a !important;
      }
      
      .irs-bar,
      .irs-bar-edge {
        background: #AFA449 !important;
        border-color: #AFA449 !important;
      }
      .irs-single,
      .irs-from,
      .irs-to {
        background: #AFA449 !important;
        color: #655D1D !important;
        font-size: 0.9rem !important;
      }
      .irs-slider {
        background: #AFA449 !important;
        border-color: #8f8838 !important;
      }
      
      body, .sidebar, .card, .dataTables_wrapper, .form-control, .btn {
        font-family: 'Inter', sans-serif !important;
        font-size: 15px !important;
      }
      
      .sidebar h2, .sidebar .h2 {
        font-size: 1.3rem !important;
      }
      
      .dataTables_wrapper .dataTable td {
        background-color: #D6D1BC !important;
        font-size: 14px !important;
        padding: 6px 8px !important;
        color: #655D1D !important;
      }
      
      .dataTables_wrapper .dataTable thead th {
        background-color: #AFA449 !important;
        color: #D6D1BC !important;
        font-weight: 700 !important;
        border-bottom: 2px solid #8f8838 !important;
        font-size: 14px !important;
        padding: 8px 10px !important;
      }
      
      #prediction_html { 
        font-size: 1rem !important;
        line-height: 1.6 !important;
      }
      
      .card { 
        padding: 0.8rem; 
        background-color: #D6D1BC; 
      }
      .card-header { 
        padding: 0.5rem 0.8rem; 
        background-color: #D6D1BC !important;
        border-bottom: 2px solid #AFA449;
        color: #655D1D;
        font-weight: 700;
        font-size: 1.1rem !important;
      }
      .sidebar .form-group { margin-bottom: 0.8rem; }
      
      .plot-header {
        font-family: 'Inter', sans-serif;
        margin-bottom: 0.8rem;
      }
      .plot-header h3 {
        font-weight: 700;
        font-size: 1.5rem !important;
        margin: 0;
        color: #655D1D;
      }
      .plot-header p {
        font-size: 1.05rem !important;
        color: #555;
        margin: 0.2rem 0 0 0;
      }
      
      .irs-min, .irs-max {
        font-size: 0.9rem !important;
      }
      
      .help-block {
        font-size: 0.95rem !important;
      }
    "))
  ),
  tags$script(HTML("
    $(document).on('shiny:value', function() {
      setTimeout(function() {
        $('.dataTables_paginate .paginate_button, .dataTables_paginate .paginate_button *').css({
          'background-color': '#AFA449 !important',
          'background-image': 'none !important',
          'border': '1px solid #8f8838 !important',
          'border-radius': '4px !important',
          'color': '#655D1D !important',
          'box-shadow': 'none !important',
          'outline': 'none !important',
          'text-decoration': 'none !important',
          'font-size': '14px !important',
          'padding': '6px 12px !important'
        });
        
        $('.dataTables_paginate .paginate_button.current, .dataTables_paginate .paginate_button.current *').css({
          'background-color': '#9c9239 !important',
          'border-color': '#7a702a !important'
        });
        
        $('.dataTables_paginate .paginate_button.disabled, .dataTables_paginate .paginate_button.disabled *').css({
          'background-color': '#D6D1BC !important',
          'opacity': '0.5 !important',
          'cursor': 'not-allowed !important'
        });
        
        $('.dataTables_paginate .paginate_button').off('mouseenter mouseleave').on({
          mouseenter: function() {
            if (!$(this).hasClass('current') && !$(this).hasClass('disabled')) {
              $(this).css({'background-color': '#9c9239 !important', 'border-color': '#7a702a !important'});
              $(this).find('*').css({'background-color': '#9c9239 !important', 'border-color': '#7a702a !important'});
            }
          },
          mouseleave: function() {
            if (!$(this).hasClass('current') && !$(this).hasClass('disabled')) {
              $(this).css({'background-color': '#AFA449 !important', 'border-color': '#8f8838 !important'});
              $(this).find('*').css({'background-color': '#AFA449 !important', 'border-color': '#8f8838 !important'});
            }
          }
        });
      }, 500);
    });
  ")),
  card(
    card_header("Статистика текста"),
    htmlOutput("prediction_html"),
    height = "auto"
  ),
  navset_card_tab(
    height = "700px",
    tabPanel(
      "График частот",
      div(class = "plot-header",
          h3("Топ самых частых слов"),
          p("Расчет Term Frequency (TF)")
      ),
      plotOutput("tf_plot", height = "550px")
    ),
    tabPanel(
      "Таблица частот",
      DTOutput("tf_table")
    ),
    tabPanel(
      "Облако слов",
      wordcloud2Output("wordcloud_plot", height = "550px")
    ),
    tabPanel(
      "График N-gram",
      div(class = "plot-header",
          h3("Топ биграмм"),
          p("Наиболее частые пары слов")
      ),
      plotlyOutput("ngram_plotly", height = "550px")
    ),
    tabPanel(
      "График частей речи",
      div(class = "plot-header",
          h3("Топ слов по частям речи"),
          p("Существительные, глаголы, прилагательные")
      ),
      plotOutput("pos_plot", height = "550px")
    ),
    tabPanel(
      "Таблица частей речи",
      DTOutput("pos_table")
    )
  )
)

# Шаг 8. Сервер
server <- function(input, output) {
  
  user_text_val <- reactiveVal(NULL)
  
  observeEvent(input$predict_btn, {
    user_text_val(input$user_text)
  })
  
  ann_data <- reactive({
    req(user_text_val())
    annotate_text(user_text_val())
  })
  
  get_text_stats <- function(text) {
    if (is.null(text) || nchar(trimws(text)) == 0) {
      return("<span style='color:red; font-size: 1.1rem;'>Введите текст для анализа.</span>")
    }
    words <- strsplit(text, "\\s+")[[1]]
    sentences <- strsplit(text, "[.!?]+")[[1]]
    unique_words <- unique(words)
    
    paste0(
      "<span style='font-size: 1.05rem;'>",
      "<strong>Слов:</strong> ", length(words), "<br>",
      "<strong>Уникальных слов:</strong> ", length(unique_words), "<br>",
      "<strong>Лексическое разнообразие (TTR):</strong> ", round(length(unique_words)/length(words), 3), "<br>",
      "<strong>Количество предложений:</strong> ", length(sentences), "<br>",
      "<strong>Средняя длина слова:</strong> ", round(mean(nchar(words)), 1), "<br>",
      "</span>"
    )
  }
  
  text_stats_data <- reactive({
    req(user_text_val())
    get_text_stats(user_text_val())
  })
  
  output$prediction_html <- renderUI({
    HTML(text_stats_data())
  })
  
  # TF график
  tf_plot_data <- reactive({
    req(user_text_val())
    analyze_tf(user_text_val(), top_n = input$top_words)
  })
  
  output$tf_plot <- renderPlot({
    tf_plot_data()
  })
  
  # TF таблица
  tf_table_data <- reactive({
    req(user_text_val())
    get_tf_table(user_text_val(), top_n = input$top_words)
  })
  
  output$tf_table <- renderDT({
    req(tf_table_data())
    datatable(
      tf_table_data(),
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollY = "450px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    return '<span style=\"font-size:14px;\">' + data + '</span>';",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'display compact',
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; font-size: 1rem; color: #655D1D;',
        'Топ слов по частоте'
      )
    ) |> 
      formatRound(columns = 'Отн. частота (TF)', digits = 4)
  })
  
  output$wordcloud_plot <- renderWordcloud2({
    ann <- ann_data()
    if (is.null(ann) || nrow(ann) == 0) {
      return(wordcloud2(data.frame(word = "Нет данных", freq = 1),
                        size = 1, color = "grey", fontFamily = "Inter"))
    }
    
    wc <- tryCatch({
      make_wordcloud(ann, top_n = 100, min_freq = 1)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(wc)) {
      freq_df <- word_freq(ann, top_n = 100, min_freq = 1)
      if (is.null(freq_df) || nrow(freq_df) < 3) {
        return(wordcloud2(
          data.frame(word = "Слишком мало слов для облака (минимум 3)", freq = 1),
          size = 1, color = "grey", fontFamily = "Inter"
        ))
      } else {
        return(wordcloud2(
          data.frame(word = "Ошибка при построении облака", freq = 1),
          size = 1, color = "grey", fontFamily = "Inter"
        ))
      }
    }
    wc
  })
  
  # N-gram
  output$ngram_plotly <- renderPlotly({
    ann <- ann_data()
    if (is.null(ann) || nrow(ann) == 0) {
      return(plotly_empty(type = "scatter", mode = "text",
                          text = "Нет данных для отображения") |> 
               layout(title = "График N-gram"))
    }
    p <- make_ngram_plot(ann, top_n = input$top_words)
    if (is.null(p)) {
      return(plotly_empty(type = "scatter", mode = "text",
                          text = "Недостаточно данных для биграмм") |> 
               layout(title = "График N-gram"))
    }
    p
  })
  
  # POS
  pos_data <- reactive({
    ann <- ann_data()
    req(ann)
    get_pos_freq(ann, top_n = input$top_words)
  })
  
  output$pos_plot <- renderPlot({
    ann <- ann_data()
    req(ann)
    plot_pos_freq(ann, top_n = input$top_words)
  })
  
  output$pos_table <- renderDT({
    df <- pos_data()
    req(df)
    if (nrow(df) == 0) {
      df <- data.frame(
        `Часть речи` = "Нет данных",
        Слово = "",
        Частота = 0,
        check.names = FALSE
      )
    } else {
      df <- df |>
        select(`Часть речи` = part_of_speech, Слово = word, Частота = freq)
    }
    datatable(
      df,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollY = "450px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    return '<span style=\"font-size:14px;\">' + data + '</span>';",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      class = 'display compact',
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; font-size: 1rem; color: #655D1D;',
        'Топ слов по частям речи'
      )
    )
  })
}

# Шаг 9. Запуск
shinyApp(ui = ui, server = server)