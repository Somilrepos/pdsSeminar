library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Specify the Reddit thread JSON URL (replace with your actual thread URL)
json_url <- "/.json"

# 1. Download JSON data with a user-agent header
res <- GET(json_url, add_headers(
  'User-Agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36"
))

# 2. Check response status
if (status_code(res) != 200) {
  stop("Failed to fetch data: HTTP status ", status_code(res))
}

# 3. Parse JSON content as a list
json_text <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json_text, simplifyDataFrame = FALSE)

# 4. Extract main post data
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

# 5. Safer Recursive extraction of comments and nested replies
extract_comments <- function(comment_list) {
  # If input is NULL or empty, return empty tibble with expected columns
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
    # Skip placeholders like "more comments" without 'body'
    if (is.null(c$data$body)) next
    
    # Create tibble for the current comment
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
    
    # Safely check for replies and recurse
    if (!is.null(c$data$replies) &&
        is.list(c$data$replies) &&
        length(c$data$replies) > 0 &&
        !identical(c$data$replies, "") &&
        !is.null(c$data$replies$data) &&
        !is.null(c$data$replies$data$children) &&
        length(c$data$replies$data$children) > 0) {
      
      replies <- c$data$replies$data$children
      replies_df <- extract_comments(replies)
      
      # Only append if non-empty
      if (nrow(replies_df) > 0) {
        flat <- append(flat, list(replies_df))
      }
    }
  }
  
  # Combine all into one tibble
  bind_rows(flat)
}

# 6. Extract comments from JSON
comments_raw <- data[[2]]$data$children
comments_df <- extract_comments(comments_raw)

# 7. Combine main post and comments into one data frame
full_df <- bind_rows(main_post_df, comments_df)

# 8. Clean the dataset by removing deleted/removed comments and trimming text
clean_df <- full_df %>%
  filter(!is.na(body), !tolower(body) %in% c("[deleted]", "[removed]")) %>%
  mutate(
    author = ifelse(is.na(author) | author == "", "[unknown]", author),
    body = trimws(gsub("\n", " ", body))  # Remove line breaks and trim spaces
  )

# 9. Inspect the cleaned dataframe
print(clean_df)

# 10. Save the cleaned dataframe to CSV
write.csv(clean_df, "redditPostX.csv", row.names = FALSE)
