library(tidyverse)

#' @title Compare two dataframe for difference
#' @description Compare two data frames for left exclude or right exclude comparison.
#' Both the data frames must have the same set of columns.
#'
#' @param x A tbl to compare.
#' @param y Another tbl to compare with.
#' @param left_exclude Informs if left exclude comparison has to be used. If F, then uses right exclude join and is same as table.compare(y, x).
#' @return Returns a tibble with the difference.
#'
#' @examples
#' table.compare(x, y)
#' table.compare(y, x)
table.compare <- function(x, y, left_exclude = T)
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
#' @param connection1 Connection string to the first database.
#' @param connection2 Connection string to the second database.
#' @param left_exclude left_exclude Informs if left exclude comparison has to be used. If F, then uses right exclude join and is same as table.compare(y, x).
#' @return Returns a tibble of differences
#'
env.compare <-
  function(query,
           connection1,
           connection2,
           left_exclude = T) {
    table.compare(
      ora.run_query(query, connection1),
      ora.run_query(query, connection2),
      left_exclude
    )
  }


#' @title Copies to clipboard
#' @description Copies an object to clipboard as a tsv table.
#'
#' @param x tbl to copy.
#' @param sep Column separator. Defaulted to \t.
#' @return Copies the object to the clipboard.
#'
#' @examples
#' clipbrd.write(ds)
#' ds %>%
#'   clipbrd.write()
clipbrd.write <- function(x, sep = "\t", quote = T)
{
  x %>%
    write.table(
      "clipboard-48016",
      sep = sep,
      quote = quote ,
      row.names = F,
      na = ""
    )
}

#' @title Copies from clipboard
#' @description Copies a tsv table from clipboard.
#'
#' @param sep Column separator. Defaulted to \t.
#' @return Gets clipboard.
#'
#' @examples
#' clipbrd.read()
#' ds <- clipbrd.read(quote = "'")
clipbrd.read <-
  function(sep = "\t",
           quote = "\"",
           stringsAsFactors = F) {
    tibble::as_data_frame(
      read.table(
        "clipboard",
        sep = sep,
        quote = quote,
        stringsAsFactors = stringsAsFactors,
        header = T
      )
    )
  }
