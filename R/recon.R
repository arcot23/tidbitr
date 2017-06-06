library(tidyverse)

#' @title Compare two dataframe to show differences highlighted
#' @description Compares two data frames to show two additional columns lhs_matches, rhs_matches to inform the number of rows that matches against x and y. These columns informs if the row is present in x or y or both. Both the data frames must have the same set of columns.
#'
#' @param x First data frame to compare.
#' @param y Second data frame to compare with.
#' @param col.names Informs the title of the new columns informs if matches exists. Defaulted to lhs_matches and rhs_matches.
#' @param orderby Resulting data frame will be ordered by this column index.
#'
#' @return Returns a tibble with the difference.
#'
#' @examples
#' Recon(list1, list2)
Recon <-
  function(x,
           y,
           col.names = c("lhs", "rhs"),
           orderby = 1)
  {
    if (length(x) != length(y))
      stop("Column count between x and y are not the same")
    if (!identical(colnames(x), colnames(y)))
      stop("Column names between x and y are not the same")
    if (!identical(sapply(x, class), sapply(y, class)))
      stop("Column types between x and y are not the same")

    z <- dplyr::union(x, y) %>%
      as.tibble() %>%
      dplyr::mutate(seq_along = seq_along(.[[1]])) %>%
      nest(-seq_along) %>%
      dplyr::mutate(x_match = map(data, ~ nrow(dplyr::intersect(., x))),
             y_match = map(data, ~ nrow(dplyr::intersect(., y)))) %>%
      unnest(x_match, y_match, data) %>%
      dplyr::select(-seq_along) %>%
      dplyr::arrange(.[[orderby+2]])
    colnames(z)[1:2] <- col.names

    cat(
      sprintf(
        "# %s: %s \U00D7 %s, %s: %s \U00D7 %s, %s \U2229 %s : %s \U00D7 %s, %s \U22C3 %s: %s \U00D7 %s, %s \U2212 %s: %s \U00D7 %s, %s \U2212 %s: %s \U00D7 %s\r\n",
        col.names[1],
        nrow(x),
        length(x),
        col.names[2],
        nrow(y),
        length(x),
        col.names[1],
        col.names[2],
        nrow(dplyr::intersect(x, y)),
        length(dplyr::intersect(x, y)),
        col.names[1],
        col.names[2],
        nrow(z),
        length(z) - 3,
        col.names[1],
        col.names[2],
        nrow(z[z[col.names[2]] != T, ]),
        length(z[z[col.names[2]] != T, ]) - 2,
        col.names[2],
        col.names[1],
        nrow(z[z[col.names[1]] != T, ]),
        length(z[z[col.names[1]] != T, ]) - 2
      )
    )

    z
  }

#' @title Compares two datasets
#' @description Merges and compares two datasets.
#'#'
#' @param x Dataset to compare.
#' @param y Dataset to compare with,
#'
#' @return Returns a dataset after merging and two column that denotes which of the merged rows exists in x or y or both.
#'
#' @examples
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

    lhs <- x
    rhs <- y
    lhs$x <- T
    rhs$y <- T
    z <- merge(lhs, rhs, all = T) %>%
      as_data_frame()

    cat(
      sprintf(
        "# x: %s \U00D7 %s, y: %s \U00D7 %s, x \U2229 y : %s \U00D7 %s, x \U22C3 y: %s \U00D7 %s, x \U2212 y: %s \U00D7 %s, y \U2212 x: %s \U00D7 %s\r\n",
        nrow(x),
        length(x),
        nrow(y),
        length(x),
        nrow(dplyr::intersect(x, y)),
        length(dplyr::intersect(x, y)),
        nrow(z),
        length(z) - 2,
        nrow(z[z$y != T, ]),
        length(z[z$y != T, ]) - 2,
        nrow(z[z$x != T, ]),
        length(z[z$x != T, ]) - 2
      )
    )
    z

  }
