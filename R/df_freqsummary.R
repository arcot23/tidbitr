#'
#' @title Frequency table of all columns
#' @description Generates a frequency distribution table of all columns in a data set.
#'
#' @param x Dataset to be processed.
#' @param records Number of rows to show in the resulting table
#' @param as return as "l" list or "d" dataset
#'
#' @return Returns a frequency table in a key/value pair format for all the columns. Number of columns returned is length of the dataset x 2.
#'
#' @example
#' FreqTable(census)
#' FreqTable(census)$State
#' FreqTable(census, 50, "d")
#' FreqTable(census, 100, "d") #will throw an error, use FreqTable(census, 100, "l") instead
FreqTable <- function(x, records = 10, as = "l") {
  len <- length(x) * 2
  s <-   x %>%
    llply(
      .fun = function(f) {
        as_data_frame(head(sort(table(f), decreasing = T), records))
      }
    )

  if (as == "l")
    s
  else
  {
    s %>%
    cbind(.[[1]]) %>%
    as_data_frame() %>%
    select(1:len)
  }
}
