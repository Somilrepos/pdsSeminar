library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

json_url <- "/.json"

res <- GET(json_url, add_headers(
  'User-Agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36"
))

if (status_code(res) != 200) {
  stop("Failed to fetch data: HTTP status ", status_code(res))
}

json_text <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json_text, simplifyDataFrame = FALSE)

post <- data[[1]]$data$children[[1]]$data

main_post_df <- tibble(
  type = "post",
  author = post$author,
  body = post$selftext,
  score = post$score,
  created_utc = as.POSIXct(post$created_utc, origin = "1970-01-01", tz = "UTC"),
  id = post$id,
  parent_id = NA_character_
)

extract_comments <- function(comment_list) {
  if (is.null(comment_list) || length(comment_list) == 0) {
    return(tibble(
      type = character(),
      author = character(),
      body = character(),
      score = numeric(),
      created_utc = as.POSIXct(character()),
      id = character(),
      parent_id = character()
    ))
  }

  flat <- list()

  for (c in comment_list) {
    if (is.null(c$data$body)) next

    current <- tibble(
      type = "comment",
      author = c$data$author,
      body = c$data$body,
      score = c$data$score,
      created_utc = as.POSIXct(c$data$created_utc, origin = "1970-01-01", tz = "UTC"),
      id = c$data$id,
      parent_id = c$data$parent_id
    )

    flat <- append(flat, list(current))

    if (!is.null(c$data$replies) &&
        is.list(c$data$replies) &&
        length(c$data$replies) > 0 &&
        !identical(c$data$replies, "") &&
        !is.null(c$data$replies$data) &&
        !is.null(c$data$replies$data$children) &&
        length(c$data$replies$data$children) > 0) {

      replies <- c$data$replies$data$children
      replies_df <- extract_comments(replies)

      if (nrow(replies_df) > 0) {
        flat <- append(flat, list(replies_df))
      }
    }
  }

  bind_rows(flat)
}

comments_raw <- data[[2]]$data$children
comments_df <- extract_comments(comments_raw)

full_df <- bind_rows(main_post_df, comments_df)

clean_df <- full_df %>%
  filter(!is.na(body), !tolower(body) %in% c("[deleted]", "[removed]")) %>%
  mutate(
    author = ifelse(is.na(author) | author == "", "[unknown]", author),
    body = trimws(gsub("\n", " ", body))
  )

print(clean_df)

write.csv(clean_df, "redditPostX.csv", row.names = FALSE)
