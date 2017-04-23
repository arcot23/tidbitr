library(plyr)

#' @title Shows a quick summary of a data frame
#' @description Shows element type, row count, na row count, unique row count, mean, min, Q1, median, Q3, max, min string length, max string length and set of samples from each of the columns in a data frame.
#'
#' @param df Data frame.
#' @param sample Number of samples from each column. Defaults to 20.
#' @param sample_delim Delimiter for each sample.
Crunch <- function(df,
                               sample = 20,
                               sample_delim = "; ")
{
  parse <- function(x) {
    list(
      typeof(x),
      length(x) ,
      length(which(is.na(x))) ,
      n_distinct(x, na.rm = T) ,
      ifelse(is.numeric(x), mean(x, na.rm = T), NA),
      ifelse(is.numeric(x), paste(quantile(
        x, seq(0, 1, 0.25), na.rm = T
      ), collapse = ", "), NA),
      ifelse(is.infinite(min(
        stringr::str_length(na.omit(x))
      )), NA, min(stringr::str_length(na.omit(
        x
      )))),
      ifelse(is.infinite(max(
        stringr::str_length(na.omit(x))
      )), NA, max(stringr::str_length(na.omit(
        x
      )))),
      paste(head(unique(na.omit(
        x
      )), sample), collapse = sample_delim)
    )
  }
  mystat <- colwise(parse)(df)
  mystat <- t(mystat)
  mystat <- cbind(rownames(mystat) , mystat)
  colnames(mystat) <-
    c(
      "Element",
      "Type",
      "Rows",
      "N/As",
      "Distinct_Rows",
      "Mean",
      "Min_Q1_Median_Q3_Max",
      "MinStrLen",
      "MaxStrLen",
      paste(sample, "Samples")
    )
  mystat
}
