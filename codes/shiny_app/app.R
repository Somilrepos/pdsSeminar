library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(tidytext)

resolve_base_dir <- function() {
  candidates <- c('.', '..', '../..', '../../..')
  for (c in candidates) {
    if (file.exists(file.path(c, 'data')) && file.info(file.path(c, 'data'))$isdir) {
      return(normalizePath(c))
    }
  }
  normalizePath('.')
}

base_dir <- resolve_base_dir()
data_dir <- file.path(base_dir, 'data')
clean_dir <- file.path(base_dir, 'data', 'cleaned')

ensure_clean_data <- function() {
  # On hosted deployments, the app bundle is read-only. Expect cleaned CSVs
  # to be included under data/cleaned in the repo. If missing, inform via console.
  needed <- c(
    'amdRev_01_clean.csv','amdRev_02_clean.csv','amdRev_03_clean.csv',
    'cpuOverall_clean.csv','cpuMulti_clean.csv','cpuSingle_clean.csv',
    'intel_q4_clean.csv','intel_fy_clean.csv','intel_segments_clean.csv',
    'intel_income_clean.csv','intel_margin_clean.csv','reddit_clean.csv'
  )
  missing <- needed[!file.exists(file.path(clean_dir, needed))]
  if (length(missing)) {
    message('Missing cleaned files: ', paste(missing, collapse=', '))
  }
}

load_cleaned <- function() {
  ensure_clean_data()
  safe_read <- function(name) {
    path <- file.path(clean_dir, name)
    if (file.exists(path)) read_csv(path, show_col_types = FALSE) else tibble()
  }
  list(
    amd_returns = safe_read('amdRev_01_clean.csv'),
    amd_financials = safe_read('amdRev_02_clean.csv'),
    amd_peer = safe_read('amdRev_03_clean.csv'),
    cpu_overall = safe_read('cpuOverall_clean.csv'),
    cpu_multi = safe_read('cpuMulti_clean.csv'),
    cpu_single = safe_read('cpuSingle_clean.csv'),
    intel_q4 = safe_read('intel_q4_clean.csv'),
    intel_fy = safe_read('intel_fy_clean.csv'),
    intel_segments = safe_read('intel_segments_clean.csv'),
    intel_income = safe_read('intel_income_clean.csv'),
    intel_margin = safe_read('intel_margin_clean.csv'),
    reddit = safe_read('reddit_clean.csv')
  )
}

format_dollar <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_, paste0('$', formatC(x, format='f', digits = digits, big.mark=',')))
}
format_percent_value <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_, paste0(formatC(x, format='f', digits = digits), '%'))
}

ui <- dashboardPage(
  dashboardHeader(title = 'Seminar Insights'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Overview', tabName = 'overview', icon = icon('chart-line')),
      menuItem('AMD', tabName = 'amd', icon = icon('microchip')),
      menuItem('CPU Market', tabName = 'cpu', icon = icon('microchip')),
      menuItem('Intel', tabName = 'intel', icon = icon('industry')),
      menuItem('Reddit', tabName = 'reddit', icon = icon('reddit'))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .box-body { overflow-x: auto; }
      table.dataTable td, table.dataTable th { white-space: normal !important; word-break: break-word; }
      .dataTables_wrapper .dataTables_scroll { overflow: auto; }
    '))),
    tabItems(
      tabItem(tabName = 'overview',
        fluidRow(
          box(width = 6, title = 'AMD Stock Momentum', status='primary', solidHeader = TRUE,
              plotlyOutput('plot_amd_returns'),
              uiOutput('insight_amd_returns')),
          box(width = 6, title = 'CPU Value Leaders', status='primary', solidHeader = TRUE,
              plotlyOutput('plot_cpu_value_top'),
              uiOutput('insight_cpu_value'))
        ),
        fluidRow(
          box(width = 6, title = 'AMD Financial Growth (YoY)', status='warning', solidHeader = TRUE,
              plotlyOutput('plot_amd_financials'),
              uiOutput('insight_amd_fin')),
          box(width = 6, title = 'Intel Segment Scale', status='warning', solidHeader = TRUE,
              plotlyOutput('plot_intel_segments'),
              uiOutput('insight_intel_segments'))
        ),
        fluidRow(
          box(width = 12, title = 'Reddit Sentiment Overview', status='success', solidHeader = TRUE,
              plotlyOutput('plot_reddit_sent_overview'),
              uiOutput('insight_reddit_sent_overview'))
        )
      ),

      tabItem(tabName = 'amd',
        fluidRow(
          box(width = 6, title = 'Returns by Period', solidHeader = TRUE,
              plotlyOutput('plot_amd_returns2')),
          box(width = 6, title = 'Peers Snapshot', solidHeader = TRUE,
              DTOutput('table_amd_peer'))
        ),
        fluidRow(
          box(width = 12, title = 'Financial Metrics YoY', solidHeader = TRUE,
              plotlyOutput('plot_amd_financials2'))
        )
      ),

      tabItem(tabName = 'cpu',
        fluidRow(
          box(width = 6, title = 'Value Score vs Price', solidHeader = TRUE,
              plotlyOutput('plot_cpu_scatter')),
          box(width = 6, title = 'Top 10 by Value', solidHeader = TRUE,
              plotlyOutput('plot_cpu_bar'))
        ),
        fluidRow(
          box(width = 6, title = 'Multi-thread Leaders', solidHeader = TRUE,
              DTOutput('table_multi')),
          box(width = 6, title = 'Single-thread Leaders', solidHeader = TRUE,
              DTOutput('table_single'))
        )
      ),

      tabItem(tabName = 'intel',
        fluidRow(
          box(width = 6, title = 'Segments (FY Revenue)', solidHeader = TRUE,
              plotlyOutput('plot_intel_segments2')),
          box(width = 6, title = 'Margins & Income (FY) (in $Millions)', solidHeader = TRUE,
              DTOutput('table_intel_margin'))
        ),
        fluidRow(
          box(width = 12, title = 'Income Statement Highlights (in $Millions)', solidHeader = TRUE,
              DTOutput('table_intel_income'))
        )
      ),

      tabItem(tabName = 'reddit',
        fluidRow(
          box(width = 12, title = 'Select Post', solidHeader = TRUE,
              uiOutput('reddit_post_selector'))
        ),
        fluidRow(
          box(width = 6, title = 'Top Sentiment Words', solidHeader = TRUE,
              plotlyOutput('plot_reddit_words')),
          box(width = 6, title = 'Brand Mentions in Comments', solidHeader = TRUE,
              plotlyOutput('plot_reddit_brands'))
        ),
        fluidRow(
          box(width = 6, title = 'Comment Sentiment', solidHeader = TRUE,
              plotlyOutput('plot_reddit_sentiment')),
          box(width = 6, title = 'Representative Comments', solidHeader = TRUE,
              DTOutput('table_reddit_comments'))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ds <- reactive(load_cleaned())

  amd_period_levels <- c('7-Day','30-Day','YTD','3-Month','6-Month','1-Year','2-Year','5-Year')

  norm_id <- function(x) {
    x <- as.character(x)
    x <- str_replace(x, '^t[0-9]_+', '')
    x
  }
  reddit_with_post_id <- reactive({
    d <- ds()$reddit
    if (nrow(d) == 0 || !all(c('id','parent_id','is_post','body') %in% names(d))) return(tibble())
    d <- d %>% mutate(
      id_norm = norm_id(id),
      parent_norm = norm_id(parent_id),
      post_id = if_else(is_post, id_norm, parent_norm)
    )
    d
  })

  build_tokens <- function(df) {
    if (nrow(df) == 0) return(tibble(post_id=character(), id=character(), word=character()))
    out <- df %>%
      select(post_id, id, body) %>%
      unnest_tokens(word, body) %>%
      anti_join(stop_words, by = 'word') %>%
      filter(!str_detect(word, '^\n*$'), str_detect(word, '[a-z]'))
    return(out)
  }

  reddit_sentiment_tokens <- reactive({
    d <- reddit_with_post_id()
    if (nrow(d) == 0) return(tibble())
    comments <- d %>% filter(!is_post)
    if (nrow(comments) == 0) return(tibble())
    toks <- build_tokens(comments)
    bing <- tidytext::get_sentiments('bing')
    toks %>% inner_join(bing, by='word')
  })

  brand_patterns <- tibble(
    brand = c('AMD','Intel','NVIDIA','Ryzen','Core i','X3D','Zen 5','Zen 4','Arrow Lake','Raptor Lake'),
    pattern = c('\\bamd\\b','\\bintel\\b','nvidia|geforce|nvda','ryzen','core\\s?i[3579]','x3d','zen\\s?5','zen\\s?4','arrow\\s+lake','raptor\\s+lake')
  )
  reddit_brands <- reactive({
    d <- reddit_with_post_id()
    if (nrow(d) == 0) return(tibble())
    comments <- d %>% filter(!is_post)
    if (nrow(comments) == 0) return(tibble())
    brand_counts <- brand_patterns %>%
      mutate(n = map_int(pattern, ~ sum(str_detect(string = tolower(dplyr::coalesce(comments$body, '')), regex(.x, ignore_case = TRUE))))) %>%
      select(brand, n)
    brand_counts
  })

  reddit_sentiment <- reactive({
    d <- reddit_with_post_id()
    if (nrow(d) == 0) return(tibble())
    comments <- d %>% filter(!is_post)
    if (nrow(comments) == 0) return(tibble())
    toks <- build_tokens(comments)
    bing <- tidytext::get_sentiments('bing')
    sent <- toks %>% inner_join(bing, by='word') %>%
      mutate(score = if_else(sentiment == 'positive', 1L, -1L)) %>%
      group_by(id) %>% summarise(sentiment = sum(score), .groups='drop') %>%
      right_join(comments %>% select(id, post_id, created_utc), by='id') %>%
      mutate(sentiment = replace_na(sentiment, 0))
    return(sent)
  })

  output$plot_amd_returns <- renderPlotly({
    d <- ds()$amd_returns
    req(nrow(d) > 0)
    d$period <- factor(d$period, levels = amd_period_levels)
    p <- ggplot(d, aes(period, percent_change)) +
      geom_col(fill = ifelse(d$percent_change >= 0, '#2ca02c', '#d62728')) +
      geom_text(aes(label = paste0(round(percent_change,1),'%')), vjust = -0.3, size = 3) +
      labs(x=NULL, y='% Change', title='AMD Price Change by Period') +
      theme_minimal()
    ggplotly(p)
  })
  

  has_cols <- function(df, cols) all(cols %in% names(df))

  output$plot_cpu_value_top <- renderPlotly({
    d <- ds()$cpu_overall
    req(nrow(d) > 0)
    req(has_cols(d, c('gaming_score','lowest_price','model')))
    d <- d %>% mutate(value_score = gaming_score/lowest_price) %>% filter(is.finite(value_score)) %>% arrange(desc(value_score)) %>% slice_head(n=10)
    p <- ggplot(d, aes(reorder(model, value_score), value_score)) +
      geom_col(fill='#1f77b4') + coord_flip() +
      labs(x='Model', y='Value Score (gaming/price)', title='Top 10 CPU Value Leaders') +
      theme_minimal()
    ggplotly(p)
  })
  

  output$plot_amd_financials <- renderPlotly({
    d <- ds()$amd_financials
    req(nrow(d) > 0)
    p <- ggplot(d, aes(reorder(metric, yoy_change), yoy_change)) +
      geom_col(fill='#ff7f0e') + coord_flip() +
      labs(x='Metric', y='YoY Change (%)', title='AMD YoY Change by Metric (Reported)') +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_intel_segments <- renderPlotly({
    d <- ds()$intel_segments
    req(nrow(d) > 0)
    p <- ggplot(d, aes(reorder(segment, fy_revenue), fy_revenue, text = paste('YoY:', fy_change,'%'))) +
      geom_col(fill='#9467bd') + coord_flip() +
      labs(x='Segment', y='FY Revenue ($B)', title='Intel FY Revenue by Segment') +
      theme_minimal()
    ggplotly(p, tooltip = c('x','y','text'))
  })
  

  output$plot_reddit_scores <- renderPlotly({ NULL })

  output$plot_reddit_sent_overview <- renderPlotly({
    sent <- reddit_sentiment()
    req(nrow(sent) > 0)
    p <- ggplot(sent, aes(x = sentiment)) +
      geom_histogram(fill = '#2ca02c', bins = 21, alpha = 0.85) +
      labs(x = 'Comment sentiment score', y = 'Count', title = 'Reddit: Comment Sentiment Distribution') +
      theme_minimal()
    ggplotly(p)
  })
  

  output$plot_amd_returns2 <- renderPlotly({
    d <- ds()$amd_returns
    req(nrow(d) > 0)
    d$period <- factor(d$period, levels = amd_period_levels)
    p <- ggplot(d, aes(period, percent_change, group=1)) + geom_line(color='#1f77b4') + geom_point() +
      labs(x=NULL, y='% Change', title='AMD Returns by Period (as of March 2025)') + theme_minimal()
    ggplotly(p)
  })
  
  output$table_amd_peer <- renderDT({
    d <- ds()$amd_peer
    req(nrow(d) > 0)
    d_fmt <- d %>% mutate(
      price = format_dollar(price, 2),
      market_cap_b = paste0('$', formatC(market_cap_b, format='f', digits=2), 'B'),
      revenue_growth = paste0(formatC(revenue_growth, format='f', digits=2), '%'),
      gross_margin = paste0(formatC(gross_margin, format='f', digits=2), '%'),
      operating_margin = paste0(formatC(operating_margin, format='f', digits=2), '%')
    )
    datatable(d_fmt,
      options = list(pageLength = 5, scrollX = TRUE, autoWidth = TRUE),
      class = 'nowrap stripe compact',
      rownames = FALSE,
      width = '100%'
    )
  })
  output$plot_amd_financials2 <- renderPlotly({
    d <- ds()$amd_financials
    req(nrow(d) > 0)
    d_long <- d %>%
      pivot_longer(cols = c(q4_2024, q3_2024, q4_2023, q3_2023), names_to = 'quarter', values_to = 'value') %>%
      mutate(quarter = recode(quarter,
                              q4_2024 = 'Q4 2024', q3_2024 = 'Q3 2024', q4_2023 = 'Q4 2023', q3_2023 = 'Q3 2023'),
             quarter = factor(quarter, levels = c('Q4 2024','Q3 2024','Q4 2023','Q3 2023')))
    p <- ggplot(d_long, aes(metric, value, fill = quarter)) +
      geom_col(position = 'dodge') +
      labs(x = 'Metric', y = 'Amount (USD)', title = 'AMD Financials by Quarter (Comparison)') +
      theme_minimal()
    ggplotly(p)
  })

  output$plot_cpu_scatter <- renderPlotly({
    d <- ds()$cpu_overall
    req(nrow(d) > 0)
    req(has_cols(d, c('gaming_score','lowest_price','Architecture','model')))
    d <- d %>% mutate(value_score = gaming_score/lowest_price) %>% filter(is.finite(value_score))
    p <- ggplot(d, aes(lowest_price, gaming_score, text=model)) +
      geom_point(aes(size=value_score, color=Architecture), alpha=0.8) +
      scale_size_continuous(name='Value Score') +
      labs(x='Price ($)', y='Gaming Score (%)', title='Value vs Performance') + theme_minimal()
    ggplotly(p, tooltip = c('text','x','y'))
  })
  output$plot_cpu_bar <- renderPlotly({
    d <- ds()$cpu_overall
    req(nrow(d) > 0)
    req(has_cols(d, c('gaming_score','lowest_price','model')))
    d <- d %>% mutate(value_score = gaming_score/lowest_price) %>% filter(is.finite(value_score)) %>% arrange(desc(value_score)) %>% slice_head(n=10)
    p <- ggplot(d, aes(reorder(model, value_score), value_score)) + geom_col(fill='#1f77b4') + coord_flip() +
      labs(x='Model', y='Value Score', title='Top 10 by Value') + theme_minimal()
    ggplotly(p)
  })
  output$table_multi <- renderDT({
    d <- ds()$cpu_multi
    req(nrow(d) > 0)
    datatable(d,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      class = 'nowrap stripe compact',
      rownames = FALSE,
      width = '100%'
    )
  })
  output$table_single <- renderDT({
    d <- ds()$cpu_single
    req(nrow(d) > 0)
    datatable(d,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      class = 'nowrap stripe compact',
      rownames = FALSE,
      width = '100%'
    )
  })

  output$plot_intel_segments2 <- renderPlotly({
    d <- ds()$intel_segments
    req(nrow(d) > 0)
    p <- ggplot(d, aes(reorder(segment, fy_revenue), fy_revenue, text=paste('YoY', fy_change,'%'))) +
      geom_col(fill='#9467bd') + coord_flip() + labs(x='Segment', y='FY Revenue ($B)', title='Intel Segments') + theme_minimal()
    ggplotly(p, tooltip = c('x','y','text'))
  })
  output$table_intel_margin <- renderDT({
    d <- ds()$intel_margin
    req(nrow(d) > 0)
    datatable(d,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      class = 'nowrap stripe compact',
      rownames = FALSE,
      width = '100%'
    )
  })
  output$table_intel_income <- renderDT({
    d <- ds()$intel_income
    req(nrow(d) > 0)
    datatable(d,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      class = 'nowrap stripe compact',
      rownames = FALSE,
      width = '100%'
    )
  })

  output$reddit_post_selector <- renderUI({
    d <- reddit_with_post_id()
    req(nrow(d) > 0)
    posts <- d %>% filter(is_post) %>% mutate(label = if_else(nchar(body) > 120, paste0(substr(body,1,120),'…'), body)) %>%
      mutate(label = if_else(label == '' | is.na(label), paste0('Post ', id_norm), label))
    choices <- setNames(posts$id_norm, posts$label)
    selectInput('reddit_post', 'Choose a post to analyze comments:', choices = choices, selected = head(posts$id_norm,1), width = '100%')
  })
  selected_comments <- reactive({
    d <- reddit_with_post_id()
    req(nrow(d) > 0, !is.null(input$reddit_post))
    d %>% filter(!is_post, post_id == input$reddit_post)
  })

  output$plot_reddit_words <- renderPlotly({
    cmts <- selected_comments(); req(nrow(cmts) > 0)
    toks <- reddit_sentiment_tokens(); req(nrow(toks) > 0)
    toks <- toks %>% semi_join(cmts %>% select(id), by = 'id')
    req(nrow(toks) > 0)
    counts <- toks %>% count(sentiment, word, sort = TRUE)
    top_pos <- counts %>% filter(sentiment == 'positive') %>% slice_head(n = 10) %>% mutate(dir = 'Positive')
    top_neg <- counts %>% filter(sentiment == 'negative') %>% slice_head(n = 10) %>% mutate(dir = 'Negative')
    top <- bind_rows(top_pos, top_neg)
    p <- ggplot(top, aes(reorder(word, n), n, fill = dir)) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = c('Positive' = '#2ca02c', 'Negative' = '#d62728')) +
      labs(x = NULL, y = 'Count', title = 'Top Sentiment Words (Selected Post)') +
      theme_minimal()
    ggplotly(p)
  })
  output$plot_reddit_brands <- renderPlotly({
    cmts <- selected_comments(); req(nrow(cmts) > 0)
    counts <- brand_patterns %>% mutate(n = map_int(pattern, ~ sum(str_detect(tolower(dplyr::coalesce(cmts$body, '')), regex(.x, ignore_case=TRUE))))) %>% select(brand, n)
    p <- ggplot(counts, aes(reorder(brand, n), n)) + geom_col(fill='#1f77b4') + coord_flip() +
      labs(x=NULL, y='Mentions', title='Brand Mentions') + theme_minimal()
    ggplotly(p)
  })
  output$plot_reddit_sentiment <- renderPlotly({
    sent <- reddit_sentiment(); req(nrow(sent) > 0, !is.null(input$reddit_post))
    sent_sel <- sent %>% filter(post_id == input$reddit_post)
    req(nrow(sent_sel) > 0)
    p <- ggplot(sent_sel, aes(y=sentiment)) + geom_boxplot(fill='#ff7f0e', alpha=0.7) +
      labs(x=NULL, y='Sentiment (bing lexicon)', title='Comment Sentiment (Selected Post)') + theme_minimal()
    ggplotly(p)
  })
  output$table_reddit_comments <- renderDT({
    cmts <- selected_comments(); req(nrow(cmts) > 0)
    show <- cmts %>% transmute(comment = if_else(nchar(body) > 140, paste0(substr(body,1,140),'…'), body))
    datatable(show,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      class = 'stripe compact',
      rownames = FALSE,
      width = '100%'
    )
  })
}
