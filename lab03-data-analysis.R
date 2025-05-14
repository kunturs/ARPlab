# Set working directory
setwd("~/Downloads/ARPlab")

# Load required libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(viridis)
library(ggrepel)

# Get all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")

# Read and combine all CSVs into one dataframe
news_data <- csv_files %>%
  lapply(read_csv) %>%
  bind_rows()

# Clean the dataset: remove missing or empty headlines and text
df_clean <- news_data %>%
  filter(!is.na(headline), headline != "",
         !is.na(text), text != "")

# Convert timestamp to Date format
df_clean$date <- as.Date(df_clean$timestamp)

# Tokenize words from the text column
tokens <- df_clean %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z]")) %>%
  anti_join(get_stopwords())

# Count word frequency by date
word_counts <- tokens %>%
  count(date, word, sort = TRUE)

# Compute TF-IDF
tfidf <- word_counts %>%
  bind_tf_idf(term = word, document = date, n = n)

# Get top 5 words by TF-IDF for each day
top_words <- tfidf %>%
  group_by(date) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup()

# Prepare labels only for most recent date per word
latest_labels <- top_words %>%
  group_by(word) %>%
  filter(date == max(date)) %>%
  ungroup()

# Plot keyword importance over time
ggplot(top_words, aes(x = date, y = tf_idf, color = word, group = word)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(data = latest_labels, aes(label = word),
                  size = 3, show.legend = FALSE, max.overlaps = 10) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Top Keywords by TF-IDF Over Time",
    subtitle = "Across all merged BBC News CSV files",
    x = "Date",
    y = "TF-IDF Score",
    color = "Keyword"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

