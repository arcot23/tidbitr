#' @title Compare two dataframe for difference
#' @description Compare two data frames for left exclude or right exclude comparison.
#' Both the data frames must have the same set of columns.
#'
#' @param x A tbl to compare.
#' @param y Another tbl to compare with.
#' @param left_exclude Informs if left exclude comparison has to be used. If F, then uses right exclude join.
#' @return Returns a tibble with the difference.
#' @examples
#' tabtools.compare(x, y)
#' tabtools.compare(y, x)
tabtools.compare <- function(x, y, left_exclude = T)
{
  if (length(x) != length(y))
    stop("Number of columns between x and y are not the same")
  if (!identical(colnames(x), colnames(y)))
    stop("Column names between x and y are not the same")
  if (!identical(sapply(tab1, class), sapply(tab2, class)))
    stop("Columns between x and y are not the same data type")
  x <- tibble::as_tibble(x)
  y <- tibble::as_tibble(y)


  if (left_exclude)
    dplyr::setdiff(x, y)
  else
    dplyr::setdiff(y, x)

}
