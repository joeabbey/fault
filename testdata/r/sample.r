# Data analysis script for processing survey results

library(dplyr)
library("tidyr")
library(ggplot2)
require(jsonlite)
require("readr")

# Namespace-qualified calls
cleaned <- dplyr::filter(raw_data, !is.na(value))
plot_obj <- ggplot2::ggplot(cleaned, ggplot2::aes(x = date, y = value))
stats::median(cleaned$value)

MAX_RETRIES <- 3
DEFAULT_TIMEOUT <- 30
API_VERSION <- "v2"

# Top-level function definitions
clean_data <- function(df, threshold = 0.5) {
  df %>%
    filter(!is.na(value)) %>%
    filter(value > threshold)
}

summarize_results <- function(df) {
  df %>%
    group_by(category) %>%
    summarize(
      mean_val = mean(value),
      sd_val = sd(value),
      n = n()
    )
}

fetch_remote_data <- function(url, retries = MAX_RETRIES) {
  for (i in seq_len(retries)) {
    result <- tryCatch(
      jsonlite::fromJSON(url),
      error = function(e) NULL
    )
    if (!is.null(result)) return(result)
  }
  stop("Failed to fetch data after ", retries, " retries")
}

# Private helper (starts with dot)
.validate_input <- function(x) {
  if (!is.data.frame(x)) stop("Input must be a data frame")
  invisible(TRUE)
}

# S4 class definition
setClass("SurveyResult", representation(
  id = "character",
  responses = "data.frame",
  metadata = "list"
))

# R6-style class
DataPipeline <- R6Class("DataPipeline",
  public = list(
    initialize = function(config) {
      self$config <- config
    },
    run = function(input) {
      cleaned <- clean_data(input)
      summarize_results(cleaned)
    }
  )
)

# setRefClass
ConfigManager <- setRefClass("ConfigManager",
  fields = list(
    settings = "list",
    path = "character"
  ),
  methods = list(
    load = function() {
      settings <<- jsonlite::fromJSON(path)
    }
  )
)

# Nested function (should not be top-level export)
outer_function <- function(x) {
  inner_helper <- function(y) {
    y * 2
  }
  inner_helper(x) + 1
}

format_output <- function(results, format = "csv") {
  switch(format,
    csv = readr::write_csv(results, "output.csv"),
    json = jsonlite::write_json(results, "output.json")
  )
}
