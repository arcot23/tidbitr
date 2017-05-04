library(tidyverse)


#' @title Compare two dataframe to show differences highlighted
#' @description Compares two data frames to show two additional columns x_match, y_match to inform the number of rows that matches against x and y. These columns informs if the row is present in x or y or both. Both the data frames must have the same set of columns.
#'
#' @param x First data frame to compare.
#' @param y Second data frame to compare with.
#'
#' @return Returns a tibble with the difference.
#'
#' @examples
#' Recon(list1, list2)
Recon <-
  function(x,
           y,
           col.names = c("lhs_matches", "rhs_matches"),
           check_duplicates_of = 1)
  {
    if (length(x) != length(y))
      stop("Column count between x and y are not the same")
    if (!identical(colnames(x), colnames(y)))
      stop("Column names between x and y are not the same")
    if (!identical(sapply(x, class), sapply(y, class)))
      stop("Column types between x and y are not the same")

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

#' @title Compares two datasets
#' @description Merges and compares two datasets.
#'#'
#' @param x Dataset to compare.
#' @param y Dataset to compare with,
#'
#' @return Returns a dataset after merging and two column that denotes which of the merged rows exists in x or y or both.
#'
#' @example
#' x %=% y
`%=%` <- #create a new binary pipe operator
  function (x, y)
  {
    if (length(x) != length(y))
      warning("Column count between x and y are not the same")
    if (!identical(colnames(x), colnames(y)))
      warning("Column names between x and y are not the same")
    if (!identical(sapply(x, class), sapply(y, class)))
      warning("Column types between x and y are not the same")

        x$x = T
    y$y = T
    merge(x, y, all = T) %>%
      as_data_frame()
  }
