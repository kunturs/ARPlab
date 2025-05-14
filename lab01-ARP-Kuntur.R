# Load ggplot2 for making plots
library(ggplot2)

# -------------------------------
# Function 1: runEDA()
# -------------------------------

# This function does analysis of message texts
# It calculates how long each message is (in characters),
# how many words it has, and how many question marks
# It also counts how many are rumors and not rumors

runEDA <- function(data, simplify = TRUE) {
  # Check: is the input a data frame?
  stopifnot(is.data.frame(data))

  # Check: does the dataset have "text" and "is_rumor" columns?
  stopifnot("text" %in% names(data), "is_rumor" %in% names(data))

  # Copy the data
  df <- data

  # If simplify = TRUE, clean the data
  if (simplify) {
    df$text <- as.character(df$text)  # convert to character
    df$is_rumor <- as.integer(ifelse(is.na(df$is_rumor), 0, df$is_rumor))  # replace NA with 0
  }

  # Calculate how long each message is
  df$text_length <- nchar(df$text)

  # Calculate number of words in each message
  df$word_count <- sapply(strsplit(df$text, "\\s+"), length)

  # Count how many question marks in each message
  df$question_marks <- sapply(gregexpr("\\?", df$text), function(x) sum(x > 0))

  # Add a label to use in plots
  df$label <- ifelse(df$is_rumor == 1, "Rumor", "Not Rumor")

  # Save results into a list
  result <- list(
    data = df,
    n_obs = nrow(df),                   # total number of rows
    n_rumor = sum(df$is_rumor == 1),    # number of rumor messages
    n_non_rumor = sum(df$is_rumor == 0) # number of non-rumor messages
  )

  # Add attributes
  attr(result, "created") <- Sys.time()         # when the result was created
  attr(result, "source") <- "dataset 2.csv"     # name of the dataset

  # Make this a special object (S3)
  class(result) <- "eda_result"

  return(result)  # give the result back
}

# -------------------------------
# Function 2: plot.eda_result()
# -------------------------------

# This function makes boxplots from the result of runEDA()

plot.eda_result <- function(x, ...) {
  # Check: is this an object from runEDA()?
  stopifnot(inherits(x, "eda_result"))

  # Get the cleaned data from the object
  df <- x$data

  # First plot: boxplot of message length
  p1 <- ggplot(df, aes(x = label, y = text_length, fill = label)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Text Length by Rumor Label", y = "Characters", x = "") +
    theme(legend.position = "none")

  # Second plot: boxplot of word count
  p2 <- ggplot(df, aes(x = label, y = word_count, fill = label)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Word Count by Rumor Label", y = "Words", x = "") +
    theme(legend.position = "none")

  # Show the plots
  print(p1)
  print(p2)
}

# Run these only if testthat is installed
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)

  # Create small sample data to test
  test_data <- data.frame(
    text = c("Hello!", "Is this real?", "Fake news?", "Yes."),
    is_rumor = c(0, 1, 1, 0),
    stringsAsFactors = FALSE
  )

  # Test 1: Function should return an S3 object
  test_that("runEDA returns correct class", {
    result <- runEDA(test_data)
    expect_s3_class(result, "eda_result")
  })

  # Test 2: Check if result includes the needed values
  test_that("EDA result includes important fields", {
    result <- runEDA(test_data)
    expect_named(result, c("data", "n_obs", "n_rumor", "n_non_rumor"))
  })

  # Test 3: Attributes should exist
  test_that("Attributes are present", {
    result <- runEDA(test_data)
    expect_true(!is.null(attr(result, "created")))
    expect_true(!is.null(attr(result, "source")))
  })

  # Test 4: Plot function works
  test_that("Plot runs with eda_result object", {
    result <- runEDA(test_data)
    expect_invisible(plot(result))
  })

  # Test 5: Word count and labels are created
  test_that("runEDA adds extra columns", {
    result <- runEDA(test_data)
    expect_true("word_count" %in% colnames(result$data))
    expect_true("label" %in% colnames(result$data))
  })

} else {
  message("Please install the 'testthat' package to run unit tests.")
}

library(ggplot2)

# Load your CSV file
setwd("~/Downloads")  # or your own folder
data <- read.csv("pheme.csv", stringsAsFactors = FALSE)

# Run EDA
eda_result <- runEDA(data)

# View structure
str(eda_result)

# Show plots
plot(eda_result)
