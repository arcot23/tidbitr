#'
#' @title Frequency table of all columns
#' @description Generates a frequency distribution table of all columns in a data set.
#'
#' @param x Dataset to be processed.
#' @param records Number of rows to show in the resulting table
#'
#' @return Returns a frequency table in a key/value pair format for all the columns. Number of columns returned is length of the dataset x 2.
#'
#' @example
#' FreqTable(census)
FreqTable <- function(x, records = 10) {
  FreqSummary <- function(x, records = 10) {
    x %>%
      lapply(
        FUN = function(x) {
          as_data_frame(head(sort(table(x), decreasing = T), records))
        }
      )
  }
  len <- length(x)*2
  x %>%
    FreqSummary(records = records) %>%
    cbind(.[[1]]) %>%
    as_data_frame() %>%
    select(1:len)
}
