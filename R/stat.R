library(plyr)

#' @title Shows a quick summary of a data frame
#' @description Shows element type, row count, na row count, unique row count, mean, min, Q1, median, Q3, max, min string length, max string length and set of samples from each of the columns in a data frame.
#'
#' @param df Data frame.
#' @param sample Number of samples from each column. Defaults to 20.
#' @param sample_delim Delimiter for each sample.
#'
#' @return
#' Stat(census)
Stat <- function(df,
                   sample = 20,
                   sample_delim = "; ")
{
  res <- colwise(function(x) {
    list(
      nrow = length(x),
      type = typeof(x),
      hasNA = length(x[is.na(x) == T]),
      min = ifelse(is.numeric(x), min(x), NA),
      max = ifelse(is.numeric(x), max(x), NA),
      mean = ifelse(is.numeric(x), mean(x), NA),
      median = ifelse(is.numeric(x), median(x), NA),
      sd = ifelse(is.numeric(x), sd(x), NA),
      minstrlen = min(stringr::str_length(x)),
      maxstrlen = max(stringr::str_length(x)),
      distinct = length(unique(x)),
      samples = paste(sample(x, min(sample, length(x))), collapse = sample_delim)
    )
  })(df) %>%
    t() %>%
    broom::tidy() %>%
    as.tibble() %>%
    unnest()

  colnames(res) <- c("names", "nrow", "type", "hasNA", "min", "max", "mean", "median", "sd", "minstrlen", "maxstrlen", "distinct", "samples")

  cat(sprintf("# input: %s \U00D7 %s\r\n", nrow(df), length(df)))
  res
}
