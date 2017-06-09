#'
#' @title Creates an Excel Spreadsheet
#' @examples
#' wxls(census)
#' wxls(census, sheetname  = "census2", append = T)
wxls <-
  function(x,
           filename = sprintf("./%s.xlsx", deparse(substitute(x))),
           sheetname = deparse(substitute(x)),
           ...) {
    xlsx::write.xlsx(x,
                     file = filename,
                     sheetName = sheetname,
                     showNA = F,
                     ...)
  }

