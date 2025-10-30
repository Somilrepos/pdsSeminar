# pdsSeminar

## Data Cleaning and Analysis Workflow

The repository now includes an end-to-end R script for cleaning the seminar datasets and generating summary insights.

### Prerequisites

* R (>= 4.0)
* Recommended packages: `readr`, `dplyr`, `stringr`, `tidyr`, `purrr`, `lubridate`, and `glue` (install via `install.packages('tidyverse')` and `install.packages('glue')`).

### Running the Pipeline

From the project root:

```bash
Rscript codes/data_clean_analysis.R
```

The script reads the raw CSV files in `data/`, writes cleaned versions to `data/cleaned/`, and exports key insight tables to `outputs/`.

### Outputs

* `data/cleaned/` — normalized CSVs for each dataset (AMD performance, CPU benchmarks, Intel financials, Reddit activity).
* `outputs/analysis_summary.csv` — headline insights derived from the cleaned data.
* `outputs/cpu_value_top5.csv` — top gaming value CPUs by price/performance.
* `outputs/reddit_top_authors.csv` — most active authors in the Reddit extracts.

## Shiny Dashboard (Interactive Insights)

Launch the interactive dashboard to explore plots, charts, and tables:

```r
shiny::runApp('codes/shiny_app')
```

The app reads from `data/cleaned`. If cleaned files are missing, it will attempt to source `codes/data_clean_analysis.R` to generate them.

Recommended packages:

```r
install.packages(c('shiny','shinydashboard','tidyverse','plotly','DT','lubridate'))
```
