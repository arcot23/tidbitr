library(plyr)

dataframe.get_stat <- function(df)
{
  mystat <-
    t(colwise(function(x) {
      list(
        typeof(x),
        length(x) ,
        length(x[is.na(x)]) ,
        length(unique(x[!is.na(x)])) ,
        ifelse(is.numeric(x), mean(x), NA),
        ifelse(is.numeric(x), paste(quantile(
          x, seq(0, 1, 0.25), na.rm = T
        ), collapse = ", "), NA),
        ifelse(is.infinite(min(
          stringr::str_length(x[!is.na(x)])
        )), NA, min(stringr::str_length(x[!is.na(x)]))),
        ifelse(is.infinite(max(
          stringr::str_length(x[!is.na(x)])
        )), NA, max(stringr::str_length(x[!is.na(x)]))),
        paste(head(unique(x[!is.na(x)]), 20), collapse = "; ")
      )
    })(df))
  colnames(mystat) <-
    c(
      "Data Type",
      "Records",
      "N/As",
      "Unique Records",
      "Mean",
      "Min, Q1, Median, Q3, Max",
      "Min String Length",
      "Max String Length",
      "20
      Samples"
    )
  mystat
}
