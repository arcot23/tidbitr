#' @title Converts an date string to a date
#' @description Converts a string in date format(dd-MMM-yy) into a R Date.
#'
#' @param x Date string to be converted.
#' @param century Informs if the string has the century included (dd-MM-yyyy). Default is F.
#' @return Returns the date string as a date
#' @examples
#' string_to_date("01-JAN-16")
#' string_to_date("01-JAN-2017", T)
string_to_date <- function(x, century = F)
{
  as.Date(x, ifelse(century, "%d-%b-%Y", "%d-%b-%y"))
}
