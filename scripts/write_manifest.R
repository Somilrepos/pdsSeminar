#!/usr/bin/env Rscript

# Generate rsconnect manifest.json for deployment on shinyapps.io / RStudio Connect.
# Run from project root: Rscript scripts/write_manifest.R

suppressPackageStartupMessages({
  if (!requireNamespace('rsconnect', quietly = TRUE)) {
    stop('Please install rsconnect: install.packages("rsconnect")')
  }
})

rsconnect::writeManifest(appDir = '.')
cat('Wrote manifest.json to project root.\n')

