#' @title Copies to clipboard
#' @description Copies an object to clipboard as a tsv table.
#'
#' @param x tbl to copy.
#' @param sep Column separator. Defaulted to tab.
#' @param quote T (default) informs that the result will be within quotes.
#' @param na Value for NAs. Default is an empty string.
#' @param row.names Informs if row names must be included.
#' @param col.names Informs if column names must be included.
#' @param ... Other parameters.
#' @return Copies the object to the clipboard.
#'
#' @examples
#' CopyToClipboard(census)
#' census %>%
#'   CopyToClipboard()
CopyToClipboard <-
  function(x,
           sep = "\t",
           quote = T,
           na = "",
           row.names = F,
           col.names = T,
           ...)
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
    cat(sprintf("# Copied %s rows\r\n", nrow(x)))
  }

#' @title Copies from clipboard
#' @description Copies a tsv table from clipboard.
#'
#' @param sep Column separator. Defaulted to tab.
#' @param quote Informs the quote character.
#' @param stringsAsFactors Informs if string has to be treated as factors.
#' @param header Informs if a header is the first row.
#' @param ... Other parameters.
#' @return Gets clipboard.
#'
#' @examples
#' CopyFromClipboard()
#' ds <- CopyFromClipboard(quote = "'")
CopyFromClipboard <-
  function(sep = "\t",
           quote = "\"",
           stringsAsFactors = F,
           header = T,
           ...) {
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
