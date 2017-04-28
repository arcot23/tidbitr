library(tidyverse)

#' @title Compare two dataframe for differences
#' @description Compare two data frames.
#' Both the data frames must have the same set of columns.
#'
#' @param x First data frame to compare.
#' @param y Second data frame to compare with.
#' @param inXbutNotY If True (default) informs to find differences in X and not in Y. If False infors to find differences in Y and not in X.
#' @return Returns a tibble with differences with three new columns a. duplicates, count of matches based on a key. b. lhs_matches, count of matches in x for the entire row, c. rhs_matches, count of matches in y for the entire row.
#'
#' @examples
#' Compare(x, y)
#' Compare(y, x)
Compare <- function(x, y, inXbutNotY = T)
{
  if (length(x) != length(y))
    stop("Number of columns between x and y are not the same")
  if (!identical(colnames(x), colnames(y)))
    stop("Column names between x and y are not the same or the order is different")
  if (!identical(sapply(x, class), sapply(y, class)))
    stop("Columns between x and y are not of the same data type")

  if (inXbutNotY)
    dplyr::setdiff(x, y)
  else
    dplyr::setdiff(y, x)

}

#' @title Compare two dataframe to show differences highlighted
#' @description Compares two data frames to show two additional columns x_match, y_match to inform the number of rows that matches against x and y. These columns informs if the row is present in x or y or both. Both the data frames must have the same set of columns.
#'
#' @param x First data frame to compare.
#' @param y Second data frame to compare with.
#'
#' @return Returns a tibble with the difference.
#'

CompareAndShowAll <-
  function(x,
           y,
           col.names = c("lhs_matches", "rhs_matches"),
           check_duplicates_of = 1)
  {
    if (length(x) != length(y))
      stop("Number of columns between x and y are not the same")
    if (!identical(colnames(x), colnames(y)))
      stop("Column names between x and y are not the same")
    if (!identical(sapply(x, class), sapply(y, class)))
      stop("Columns between x and y are not of the same data type")

    all <- dplyr::union(x, y)

    df <- data.frame()
    for (i in 1:nrow(all))
    {
      df <-
        rbind(df,
              cbind (
                all[i,],
                n_duplicates = nrow(filter(all, all[, check_duplicates_of] == c(all[i, check_duplicates_of]))),
                x_match = nrow(dplyr::intersect(all[i,], x)),
                y_match = nrow(dplyr::intersect(all[i,], y))
              ))
    }

    colnames(df)[length(df) - 1] <-  col.names[1]
    colnames(df)[length(df)] <-  col.names[2]
    tibble::as_data_frame(df) %>%
      arrange(.[[check_duplicates_of]])
  }

`%=%` <- #create a new binary pipe operator
  function (x, y)
  {
    x$x = T
    y$y = T
    merge(x, y, all = T) %>%
      as_data_frame()
  }
