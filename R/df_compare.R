library(tidyverse)

#' @title Compare two dataframe for difference
#' @description Compare two data frames for left exclude or right exclude comparison.
#' Both the data frames must have the same set of columns.
#'
#' @param x First data frame to compare.
#' @param y Second data frame to compare with.
#' @param left_exclude Informs if left exclude comparison has to be used. If F, then uses right exclude join and is same as compare(y, x).
#' @return Returns a tibble with the difference.
#'
#' @examples
#' compare(x, y)
#' compare(y, x)
compare <- function(x, y, left_exclude = T)
{
  if (length(x) != length(y))
    stop("Number of columns between x and y are not the same")
  if (!identical(colnames(x), colnames(y)))
    stop("Column names between x and y are not the same")
  if (!identical(sapply(x, class), sapply(y, class)))
    stop("Columns between x and y are not the same data type")

  if (left_exclude)
    dplyr::setdiff(x, y)
  else
    dplyr::setdiff(y, x)

}


#' @title Compares two SQL resultsets from two database environments
#' @description Takes a SQL statement as an input and executes the SQL in two database environments, then compares the SQL resultset for differences.
#'
#' @param query SQL Query.
#' @param env1 Environment string to the first database.
#' @param env2 Environment string to the second database.
#' @param left_exclude left_exclude Informs if left exclude comparison has to be used. If F, then uses right exclude join and is same as compare(y, x).
#' @return Returns a tibble of differences
#'
compare_ora_dataset <-
  function(query,
           env1,
           env2,
           left_exclude = T) {
    compare(
      ora_run(query, env1),
      ora_run(query, env2),
      left_exclude
    )
  }
