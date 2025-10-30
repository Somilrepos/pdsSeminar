library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)
library(glue)

data_dir <- file.path('data')
clean_dir <- file.path(data_dir, 'cleaned')

dir.create(clean_dir, showWarnings = FALSE, recursive = TRUE)

parse_currency <- function(x) {
  x %>%
    str_replace_all('[$,]', '') %>%
    str_replace_all('\\(', '-') %>%
    str_replace_all('\\)', '') %>%
    str_to_lower() %>%
    
    str_replace_all('billion|million|usd', '') %>%
    str_trim() %>%
    str_extract('-?[0-9]+\\.?[0-9]*') %>%
    na_if('') %>%
    as.numeric()
}

parse_percent <- function(x) {
  x %>%
    str_replace_all('%', '') %>%
    str_replace_all(',', '') %>%
    str_trim() %>%
    str_extract('-?[0-9]+\\.?[0-9]*') %>%
    na_if('') %>%
    as.numeric()
}

parse_directional_percent <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all('down ', '-') %>%
    str_replace_all('up ', '') %>%
    str_replace_all('flat', '0') %>%
    parse_percent()
}

split_cpu_price <- function(x) {
  direct <- str_match(x, '^\\$?([0-9,]+)\\s*-\\s*(.*)$')
  if (!is.na(direct[1, 1])) {
    return(tibble(model = str_trim(direct[1, 3]), price = parse_currency(direct[1, 2])))
  }
  paren <- str_match(x, '^(.*)\\s*\\((\\$[0-9,.]+)\\)$')
  if (!is.na(paren[1, 1])) {
    return(tibble(model = str_trim(paren[1, 2]), price = parse_currency(paren[1, 3])))
  }
  tibble(model = str_trim(x), price = NA_real_)
}

format_dollar <- function(x, digits = 0) {
  ifelse(is.na(x), NA_character_, paste0('$', formatC(x, format = 'f', digits = digits, big.mark = ',')))
}

format_percent_value <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_, paste0(formatC(x, format = 'f', digits = digits), '%'))
}

clean_core_string <- function(x) {
  parts <- str_split(x, '\\(') %>% map_chr(1) %>% str_trim()
  cores <- str_extract(parts, '^[0-9]+') %>% as.numeric()
  threads <- str_extract(parts, '(?<=/\\s?)[0-9]+') %>% as.numeric()
  tibble(cores = cores, threads = threads)
}

combine_cpu_specs <- function(df, score_col) {
  specs <- clean_core_string(df[["Cores/Threads (P+E)"]])
  speeds <- df[["Base/Boost GHz"]] %>%
    str_split('/') %>%
    map(~ as.numeric(str_trim(.x)))
  base_speed <- map_dbl(speeds, 1)
  boost_speed <- map_dbl(speeds, length)
  df %>%
    mutate(
      base_ghz = base_speed,
      boost_ghz = boost_speed,
      cores = specs$cores,
      threads = specs$threads,
      score = parse_percent(.data[[score_col]]),
      price_low = parse_currency(`Lowest Price`),
      value_score = score / price_low
    )
}

amd_returns <- read_csv(file.path(data_dir, 'amdRev_01.csv'),
                        col_names = c('period', 'absolute_change', 'percent_change'),
                        show_col_types = FALSE)

amd_returns_clean <- amd_returns %>%
  mutate(
    absolute_change = parse_currency(absolute_change),
    percent_change = parse_percent(percent_change)
  )

write_csv(amd_returns_clean, file.path(clean_dir, 'amdRev_01_clean.csv'))

amd_financials <- read_csv(file.path(data_dir, 'amdRev_02.csv'),
                           col_names = c('metric', 'q4_2024', 'q3_2024', 'q4_2023', 'q3_2023', 'yoy_change'),
                           show_col_types = FALSE)

amd_financials_clean <- amd_financials %>%
  mutate(
    across(starts_with('q'), parse_currency),
    yoy_change = parse_percent(yoy_change)
  )

write_csv(amd_financials_clean, file.path(clean_dir, 'amdRev_02_clean.csv'))

amd_peer <- read_csv(file.path(data_dir, 'amdRev_03.csv'),
                     col_names = c('company', 'price', 'market_cap_b', 'pe_ratio', 'revenue_growth', 'gross_margin', 'operating_margin'),
                     show_col_types = FALSE)

amd_peer_clean <- amd_peer %>%
  mutate(
    price = parse_currency(price),
    market_cap_b = parse_currency(market_cap_b),
    pe_ratio = as.numeric(pe_ratio),
    revenue_growth = parse_percent(revenue_growth),
    gross_margin = parse_percent(gross_margin),
    operating_margin = parse_percent(operating_margin)
  )

write_csv(amd_peer_clean, file.path(clean_dir, 'amdRev_03_clean.csv'))

cpu_overall <- read_csv(file.path(data_dir, 'cpuOverall.csv'), show_col_types = FALSE)

cpu_overall_clean <- cpu_overall %>%
  separate(`Product / (MSRP)`, into = c('model', 'msrp_raw'), sep = '\\s*\\(', fill = 'right') %>%
  mutate(
    msrp = parse_currency(str_replace(msrp_raw, '\\)$', '')),
    gaming_score = parse_percent(`1080p Gaming Score`),
    lowest_price = parse_currency(`Lowest Price`),
    price_gap = lowest_price - msrp,
    value_score = gaming_score / lowest_price
  ) %>%
  select(model, msrp, lowest_price, gaming_score, Architecture, `Cores/Threads (P+E)`, `Base/Boost GHz`, `TDP / PBP / MTP`, price_gap, value_score)

write_csv(cpu_overall_clean, file.path(clean_dir, 'cpuOverall_clean.csv'))

cpu_multi <- read_csv(file.path(data_dir, 'cpuMulti.csv'), show_col_types = FALSE)

cpu_multi_clean <- cpu_multi %>%
  rename(product = any_of(c('X1', '...1'))) %>%
  mutate(
    list_cols = map(product, ~ split_cpu_price(.x)),
    model = map_chr(list_cols, 'model'),
    price = map_dbl(list_cols, 'price'),
    score = parse_percent(`Multi-Threaded App Score`),
    architecture = `Architecture`,
    cores_threads = `Cores/Threads (P+E)`,
    base_boost = `Base/Boost GHz`,
    tdp = `TDP / PBP / MTP`
  ) %>%
  select(model, price, score, architecture, cores_threads, base_boost, tdp)

write_csv(cpu_multi_clean, file.path(clean_dir, 'cpuMulti_clean.csv'))

cpu_single <- read_csv(file.path(data_dir, 'cpuSingle.csv'), show_col_types = FALSE)

cpu_single_clean <- cpu_single %>%
  rename(product = tidyselect::any_of(c('X1', '...1'))) %>%
  mutate(
    list_cols = map(product, ~ split_cpu_price(.x)),
    model = map_chr(list_cols, 'model'),
    price = map_dbl(list_cols, 'price'),
    score = parse_percent(`Single-Threaded App Score`),
    architecture = `Architecture`,
    cores_threads = `Cores/Threads (P+E)`,
    base_boost = `Base/Boost GHz`,
    tdp = `TDP / PBP / MTP`
  ) %>%
  select(model, price, score, architecture, cores_threads, base_boost, tdp)

write_csv(cpu_single_clean, file.path(clean_dir, 'cpuSingle_clean.csv'))

intel_q4 <- read_csv(file.path(data_dir, 'intel_1.csv'), show_col_types = FALSE) %>%
  rename(metric = ...1)

colnames(intel_q4) <- c('metric', 'gaap_current', 'gaap_prior', 'gaap_change', 'nongaap_current', 'nongaap_prior', 'nongaap_change')
intel_q4_clean <- intel_q4 %>%
  slice(-1) %>%
  mutate(
    gaap_current = parse_currency(gaap_current),
    gaap_prior = parse_currency(gaap_prior),
    gaap_change = parse_directional_percent(gaap_change),
    nongaap_current = parse_currency(nongaap_current),
    nongaap_prior = parse_currency(nongaap_prior),
    nongaap_change = parse_directional_percent(nongaap_change)
  )

write_csv(intel_q4_clean, file.path(clean_dir, 'intel_q4_clean.csv'))

intel_fy <- read_csv(file.path(data_dir, 'intel_2.csv'), show_col_types = FALSE) %>%
  rename(metric = ...1)

colnames(intel_fy) <- c('metric', 'gaap_current', 'gaap_prior', 'gaap_change', 'nongaap_current', 'nongaap_prior', 'nongaap_change')
intel_fy_clean <- intel_fy %>%
  slice(-1) %>%
  mutate(
    gaap_current = parse_currency(gaap_current),
    gaap_prior = parse_currency(gaap_prior),
    gaap_change = parse_directional_percent(gaap_change),
    nongaap_current = parse_currency(nongaap_current),
    nongaap_prior = parse_currency(nongaap_prior),
    nongaap_change = parse_directional_percent(nongaap_change)
  )

write_csv(intel_fy_clean, file.path(clean_dir, 'intel_fy_clean.csv'))

intel_segments <- read_csv(file.path(data_dir, 'intel_3.csv'), show_col_types = FALSE) %>%
  rename(segment = `Intel Products:`) %>%
  slice(-1) %>%
  rename(q4_revenue = ...2, q4_change = ...3, fy_revenue = ...4, fy_change = ...5) %>%
  mutate(
    q4_revenue = parse_currency(q4_revenue),
    q4_change = parse_directional_percent(q4_change),
    fy_revenue = parse_currency(fy_revenue),
    fy_change = parse_directional_percent(fy_change)
  )

write_csv(intel_segments, file.path(clean_dir, 'intel_segments_clean.csv'))

intel_income_raw <- read_csv(file.path(data_dir, 'intel_4.csv'), show_col_types = FALSE)

intel_income_clean <- intel_income_raw %>%
  rename(metric = `(In Millions, Except Per Share Amounts; Unaudited)`,
         q4_pair = `Dec 28, 2024 Dec 30, 2023`,
         fy_2024 = `Dec 28, 2024`,
         fy_2023 = `Dec 30, 2023`) %>%
  mutate(
    q4_numbers = str_extract_all(q4_pair, '-?[0-9,]+\\.?[0-9]*'),
    q4_2024 = map_dbl(q4_numbers, ~ if (length(.x) >= 1) parse_currency(.x[1]) else NA_real_),
    q4_2023 = map_dbl(q4_numbers, ~ if (length(.x) >= 2) parse_currency(.x[2]) else NA_real_),
    fy_2024 = parse_currency(fy_2024),
    fy_2023 = parse_currency(fy_2023)
  ) %>%
  select(metric, q4_2024, q4_2023, fy_2024, fy_2023)

write_csv(intel_income_clean, file.path(clean_dir, 'intel_income_clean.csv'))

intel_balance_raw <- read_csv(file.path(data_dir, 'intel_5.csv'), show_col_types = FALSE)

intel_balance_clean <- intel_balance_raw %>%
  rename(metric = Assets, fy_2024 = ...3, fy_2023 = ...5) %>%
  slice(-1) %>%
  mutate(
    fy_2024 = parse_currency(fy_2024),
    fy_2023 = parse_currency(fy_2023)
  ) %>%
  select(metric, fy_2024, fy_2023)

write_csv(intel_balance_clean, file.path(clean_dir, 'intel_balance_clean.csv'))

intel_margin_raw <- read_csv(file.path(data_dir, 'intel_6.csv'), show_col_types = FALSE)

intel_margin_clean <- intel_margin_raw %>%
  rename(metric = `(In Millions, Except Per Share Amounts; Unaudited)`,
         q4_2024 = `Dec 28, 2024...2`,
         q4_2023 = `Dec 30, 2023...3`,
         fy_2024 = `Dec 28, 2024...4`,
         fy_2023 = `Dec 30, 2023...5`) %>%
  mutate(across(-metric, parse_currency))

write_csv(intel_margin_clean, file.path(clean_dir, 'intel_margin_clean.csv'))

reddit_files <- list.files(data_dir, pattern = 'redditPost.*\\.csv$', full.names = TRUE)

reddit_data <- reddit_files %>%
  set_names(basename(reddit_files)) %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE, na = c('', 'NA')), .id = 'source') %>%
  mutate(source = basename(source))

reddit_clean <- reddit_data %>%
  mutate(
    created_utc = ymd_hms(created_utc, quiet = TRUE),
    score = as.integer(score),
    is_post = type == 'post'
  )

write_csv(reddit_clean, file.path(clean_dir, 'reddit_clean.csv'))