#' @title Copies to clipboard
#' @description Copies an object to clipboard as a tsv table.
#'
#' @param x tbl to copy.
#' @param sep Column separator. Defaulted to \t.
#' @return Copies the object to the clipboard.
#'
#' @examples
#' CopyToClipboard(census)
#' census %>%
#'   CopyToClipboard()
CopyToClipboard <- function(x, sep = "\t", quote = T, na = "", row.names = F, col.names = T, ...)
{
  x %>%
    write.table(
      "clipboard-48016",
      sep = sep,
      quote = quote,
      na = na,
      row.names = row.names,
      col.names = col.names,
      ...
    )
}

#' @title Copies from clipboard
#' @description Copies a tsv table from clipboard.
#'
#' @param sep Column separator. Defaulted to \t.
#' @return Gets clipboard.
#'
#' @examples
#' CopyFromClipboard()
#' ds <- CopyFromClipboard(quote = "'")
CopyFromClipboard <-
  function(sep = "\t",
           quote = "\"",
           stringsAsFactors = F,
           header = T, ...) {
    tibble::as_data_frame(
      read.table(
        "clipboard",
        sep = sep,
        quote = quote,
        stringsAsFactors = stringsAsFactors,
        header = header,
        ...
      )
    )
  }
