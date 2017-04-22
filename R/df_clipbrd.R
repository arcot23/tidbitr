#' @title Copies to clipboard
#' @description Copies an object to clipboard as a tsv table.
#'
#' @param x tbl to copy.
#' @param sep Column separator. Defaulted to \t.
#' @return Copies the object to the clipboard.
#'
#' @examples
#' clipbrd_write(ds)
#' ds %>%
#'   clipbrd_write()
clipbrd_write <- function(x, sep = "\t", quote = T)
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
#' clipbrd_read()
#' ds <- clipbrd_read(quote = "'")
clipbrd_read <-
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
