need <- c(
  "tidyverse","janitor","stringr","purrr","lubridate","scales",
  "rlang","fs"
)
to_get <- need[!(need %in% installed.packages()[,"Package"])]
if (length(to_get)) install.packages(to_get, dependencies = TRUE)
invisible(lapply(need, library, character.only = TRUE))

tabulizer_ok <- requireNamespace("tabulizer", quietly = TRUE)
pdftools_ok  <- requireNamespace("pdftools",  quietly = TRUE)

pdf_file <- "C:/Users/somil/Downloads/Intel-Core-Desktop-Boxed-Processors-Comparison-Chart.pdf"

if (!file.exists(pdf_file)) {
  stop("Couldn't find the PDF: ", pdf_file, "\nPut it in getwd(): ", getwd())
}

extract_tables_safely <- function(pdf_path) {
  if (!tabulizer_ok) {
    msg <- paste0(
      "Package `tabulizer` not available.\n",
      "Install Java (https://adoptium.net) then in R:\n",
      "  install.packages('rJava')\n",
      "  remotes::install_github('ropensci/tabulizer')\n",
      "After that, re-run this script."
    )
    stop(msg)
  }
  message("Extracting tables with tabulizer (this can take ~30–90 seconds for multi-page PDFs)...")
  tabs <- tabulizer::extract_tables(
    file = pdf_path,
    pages = NULL,         # all pages
    method = "stream",    # works well for column-aligned specs
    guess  = TRUE
  )
  tabs <- tabs %>%
    purrr::map(~as_tibble(.x, .name_repair = "minimal")) %>%
    purrr::discard(~ncol(.x) == 0 || nrow(.x) == 0)
  tabs
}

tabs <- extract_tables_safely(pdf_file)


canon_header <- c(
  "Processor Number","Intel Product Brand","Performance Tier","Generation",
  "Year Launched","# of Cores","# of P-cores","# of E-cores","# of Threads",
  "Max Turbo Frequency (GHz)","Performance-core Base Frequency (GHz)",
  "Efficient-core Base Frequency (GHz)","Processor Base Frequency (GHz)",
  "Cache (MB)","Processor Base Power (W)",
  "Max Memory Size (dependent on memory type) GB","Memory Types (MT/s)",
  "Max # of PCI Express Lanes","Supported Socket","Intel Processor Graphics",
  "Graphics Max Dynamic Frequency (GHz)","Intel Turbo Boost Max Technology 3.0",
  "Intel vPro Platform Eligibility","Max Resolution for HDMI, DP, eDP"
)

clean_header_text <- function(x) {
  x %>%
    str_replace_all("\uFFFE|\uFEFF|\u200B|\u00AD", "") %>%    
    str_replace_all("P.?cores", "P-cores") %>%
    str_replace_all("E.?cores", "E-cores") %>%
    str_squish()
}

guess_header <- function(tbl) {
  head_rows <- tbl %>% head(6)
  scores <- apply(head_rows, 1, function(r) {
    txt <- paste(r, collapse = " ")
    sum(str_detect(tolower(txt),
                   c("processor", "brand", "generation", "launched", "cores", "threads",
                     "turbo", "frequency", "cache", "power", "memory", "socket", "graphics", "hdmi", "dp", "edp")))
  })
  idx <- which.max(scores)
  head_rows[idx, , drop = FALSE]
}

hdr_raw <- guess_header(tabs[[1]]) %>% unlist(use.names = FALSE)
hdr_raw <- clean_header_text(hdr_raw)

if (length(hdr_raw) < 10) hdr_raw <- canon_header

fix_length <- function(x, target_len) {
  if (length(x) < target_len) c(x, rep(NA, target_len - length(x)))
  else x[seq_len(target_len)]
}
hdr_fixed <- fix_length(hdr_raw, length(canon_header))

harmonize_table <- function(tbl) {
  tbl <- tbl %>% mutate(across(everything(), ~clean_header_text(as.character(.x))))
  colnames(tbl) <- paste0("V", seq_len(ncol(tbl)))
  first_row_txt <- paste(tbl[1,] %>% unlist(), collapse = " ")
  looks_like_header <- str_detect(tolower(first_row_txt), "processor") &&
    str_detect(tolower(first_row_txt), "cores")
  if (looks_like_header) tbl <- tbl[-1, , drop = FALSE]

  if (ncol(tbl) != length(canon_header)) {
    if (ncol(tbl) > length(canon_header)) {
      keep <- tbl[, seq_len(length(canon_header)-1), drop = FALSE]
      tailmerged <- tbl[, seq(length(canon_header), ncol(tbl)), drop = FALSE] %>%
        unite("maxres_merge", everything(), sep = " ", na.rm = TRUE)
      tbl <- bind_cols(keep, tailmerged)
    } else {
      pad <- matrix(NA_character_, nrow(tbl), length(canon_header) - ncol(tbl))
      tbl <- as_tibble(cbind(tbl, pad))
    }
  }
  colnames(tbl) <- canon_header
  tbl
}

cpu_raw <- tabs %>% purrr::map(harmonize_table) %>% bind_rows()

cpu_raw <- cpu_raw %>%
  filter(
    !if_any(everything(), ~str_detect(.x, "^\\s*Page\\s*\\d+")) ,
    !if_any(everything(), ~str_detect(.x, "^\\s*(HDMI:|DP:|eDP:)"))
  )

looks_like_cpu <- function(x) {
  str_detect(x, "^(i[3579]-\\d{4,5}[A-Z]{0,2}|\\d{3}[A-Z]?)$")
}
cpu_raw <- cpu_raw %>% filter(looks_like_cpu(`Processor Number`) | !is.na(`Year Launched`))

num <- readr::parse_number

tfy <- function(x) {
  x <- tolower(x)
  dplyr::case_when(
    str_detect(x, "yes|true|y") ~ TRUE,
    str_detect(x, "no|false|n") ~ FALSE,
    TRUE ~ NA
  )
}

extract_memory_mtps <- function(s) {
  d5 <- str_match(s, "(?i)DDR5\\s*([\\d,\\.]+)\\s*MT/s")[,2] %>% num()
  d4 <- str_match(s, "(?i)DDR4\\s*([\\d,\\.]+)\\s*MT/s")[,2] %>% num()
  tibble(ddr5_mtps = d5, ddr4_mtps = d4)
}

mem_speeds <- extract_memory_mtps(cpu_raw$`Memory Types (MT/s)`)

cpu <- cpu_raw %>%
  mutate(
    across(
      c("# of Cores","# of P-cores","# of E-cores","# of Threads","Year Launched",
        "Max # of PCI Express Lanes"),
      ~suppressWarnings(as.integer(num(.x)))
    ),
    `Max Turbo Frequency (GHz)`                    = num(`Max Turbo Frequency (GHz)`),
    `Performance-core Base Frequency (GHz)`        = num(`Performance-core Base Frequency (GHz)`),
    `Efficient-core Base Frequency (GHz)`          = num(`Efficient-core Base Frequency (GHz)`),
    `Processor Base Frequency (GHz)`               = num(`Processor Base Frequency (GHz)`),
    `Cache (MB)`                                   = num(`Cache (MB)`),
    `Processor Base Power (W)`                     = num(`Processor Base Power (W)`),
    `Max Memory Size (dependent on memory type) GB`= num(`Max Memory Size (dependent on memory type) GB`),
    `Graphics Max Dynamic Frequency (GHz)`         = num(`Graphics Max Dynamic Frequency (GHz)`),
    `Intel Turbo Boost Max Technology 3.0`         = tfy(`Intel Turbo Boost Max Technology 3.0`),
    `Intel vPro Platform Eligibility`              = tfy(`Intel vPro Platform Eligibility`),
    has_integrated_graphics                        = if_else(is.na(`Intel Processor Graphics`) | `Intel Processor Graphics`=="No", FALSE, TRUE),
    generation                                     = as.integer(num(Generation)),
    brand_slim                                     = str_remove(`Intel Product Brand`, "Intel®\\s*") %>% str_squish()
  ) %>%
  bind_cols(mem_speeds) %>%
  clean_names()

cpu <- cpu %>% filter(!is.na(processor_number) | !is.na(year_launched))

fs::dir_create("data")
readr::write_csv(cpu, "data/intel_desktop_clean.csv")
message("Saved cleaned dataset to data/intel_desktop_clean.csv")

cores_year <- cpu %>%
  filter(!is.na(year_launched)) %>%
  group_by(year_launched) %>%
  summarise(
    median_cores   = median(x = `x_of_cores`, na.rm = TRUE),
    median_threads = median(x = `x_of_threads`, na.rm = TRUE),
    n_models       = n(),
    .groups = "drop"
  )

tdp_vs_threads <- cpu %>%
  filter(!is.na(processor_base_power_w), !is.na(x_of_threads))

mem_year <- cpu %>%
  filter(!is.na(year_launched) & (!is.na(ddr5_mtps) | !is.na(ddr4_mtps))) %>%
  summarise(
    ddr5 = median(ddr5_mtps, na.rm = TRUE),
    ddr4 = median(ddr4_mtps, na.rm = TRUE),
    .by = year_launched
  ) %>%
  pivot_longer(cols = c(ddr5, ddr4), names_to = "memory", values_to = "mtps")

top_turbo <- cpu %>%
  filter(!is.na(max_turbo_frequency_ghz)) %>%
  slice_max(max_turbo_frequency_ghz, n = 15) %>%
  mutate(processor_number = fct_reorder(processor_number, max_turbo_frequency_ghz))

pe_year <- cpu %>%
  filter(!is.na(year_launched)) %>%
  summarise(
    p = median(x_of_p_cores, na.rm = TRUE),
    e = median(x_of_e_cores, na.rm = TRUE),
    .by = year_launched
  ) %>%
  pivot_longer(c(p,e), names_to = "core_type", values_to = "median_count")

fs::dir_create("plots")

p1 <- ggplot(cores_year, aes(year_launched)) +
  geom_line(aes(y = median_cores)) +
  geom_point(aes(y = median_cores)) +
  geom_line(aes(y = median_threads), linetype = 2) +
  geom_point(aes(y = median_threads), shape = 21, fill = NA) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "Median Cores & Threads by Launch Year",
    x = "Year launched", y = "Count",
    caption = "Dashed = Threads; Solid = Cores"
  ) +
  theme_minimal()
ggsave("plots/01_cores_threads_by_year.png", p1, width = 8, height = 5, dpi = 160)

p2 <- ggplot(tdp_vs_threads, aes(x = x_of_threads, y = processor_base_power_w)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Processor Base Power vs Threads",
    x = "Threads", y = "Base power (W)"
  ) +
  theme_minimal()
ggsave("plots/02_tdp_vs_threads.png", p2, width = 7, height = 5, dpi = 160)

p3 <- ggplot(mem_year, aes(x = year_launched, y = mtps, group = memory)) +
  geom_line() + geom_point() +
  facet_wrap(~memory, scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "Median DDR5 / DDR4 MT/s by Launch Year",
    x = "Year launched", y = "MT/s"
  ) +
  theme_minimal()
ggsave("plots/03_memory_mtps_trend.png", p3, width = 8, height = 5, dpi = 160)

p4 <- ggplot(top_turbo, aes(x = processor_number, y = max_turbo_frequency_ghz)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 Processors by Max Turbo Frequency",
    x = NULL, y = "Max turbo (GHz)"
  ) +
  theme_minimal()
ggsave("plots/04_top_turbo.png", p4, width = 7, height = 6, dpi = 160)

p5 <- ggplot(pe_year, aes(x = year_launched, y = median_count, group = core_type)) +
  geom_line() + geom_point() +
  facet_wrap(~core_type, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "Median P-core and E-core Counts by Launch Year",
    x = "Year launched", y = "Median core count"
  ) +
  theme_minimal()
ggsave("plots/05_p_vs_e_core_trend.png", p5, width = 9, height = 4.8, dpi = 160)

message("Done. Plots saved in the 'plots/' folder and clean CSV in 'data/'.")