# Minimal scrape: grab raw tables, return first as a data.frame (no cleaning)

library(rvest)

url <- "https://www.tomshardware.com/reviews/cpu-hierarchy,4312.html"

page <- read_html(url)

# All HTML tables on the page (as-is)
tables <- page |>
  html_elements("table") |>
  html_table(fill = TRUE)

# Single dataframe (first table on the page)
df <- tables[[1]]
df2 <- tables[[2]]
df3 <- tables[[3]]
# Inspect
print(df)
print(df2)
print(df3)

write.csv(df, "cpuOverall.csv", row.names = FALSE)
write.csv(df2, "cpuSingle.csv", row.names = FALSE)
write.csv(df3, "cpuMulti.csv", row.names = FALSE)


