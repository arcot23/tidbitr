FreqSummary <- function(x) {
  x %>%
    lapply(
      FUN = function(x) {
        as_data_frame(head(sort(table(x), decreasing = T), 5))
      }
    )
}
