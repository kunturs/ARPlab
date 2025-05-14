library(rvest)
library(dplyr)
library(readr)

# Set working directory
setwd("~/Downloads/ARPlab")

# Current timestamp
now <- Sys.time()
now_str <- format(now, "%Y-%m-%d_%H-%M-%S")

# Get links from BBC News front page
page <- read_html("https://www.bbc.com/news")
links <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  na.omit()

# Filter article links only under /news and avoid videos/lives
article_links <- unique(links[grepl("^/news", links) & !grepl("live|av|video", links)])
article_links <- paste0("https://www.bbc.com", article_links)

# Scrape function
scrape_article <- function(url) {
  tryCatch({
    page <- read_html(url)
    headline <- page %>% html_element('div[data-component="headline-block"] h1') %>% html_text(trim = TRUE)
    text <- page %>% html_elements('div[data-component="text-block"] p') %>% html_text(trim = TRUE) %>% paste(collapse = "\n\n")
    data.frame(headline = headline, text = text, timestamp = now)
  }, error = function(e) NULL)
}

# Collect up to 100 articles
news_list <- list()
for (url in article_links) {
  if (length(news_list) >= 100) break
  result <- scrape_article(url)
  if (!is.null(result)) news_list[[length(news_list) + 1]] <- result
}

# Combine and save
news_df <- bind_rows(news_list)
write_csv(news_df, paste0("bbc_news_", now_str, ".csv"))

cat("✅ Done. Articles saved to: bbc_news_", now_str, ".csv\n", sep = "")



