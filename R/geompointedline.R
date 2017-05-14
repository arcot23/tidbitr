#' @title Creates a pointed lined on ggplot
#' @examples
#' ggplot() +
#'  geom_pointedline(
#'    data = filter(LifeExpectancy, Country.Code == "WLD") %>% drop_na(),
#'    aes(x = Year, y = Expectancy, color = Country.Code)
#'  )  +
#'    geom_pointedline(
#'      data = filter(LifeExpectancy, Country.Code == "USA")  %>% drop_na(),
#'      aes(x = Year, y = Expectancy, color = Country.Code)
#'    )
geom_pointedline <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           show.legend = NA,
           inherit.aes = TRUE,
           size = 1.5,
           ...) {
    list(
      ggplot2::layer(
        stat = stat,
        data = data,
        mapping = mapping,
        geom = "line",
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(size = size / 2, ...)
      ),
      ggplot2::layer(
        stat = stat,
        data = data,
        mapping = mapping,
        geom = "point",
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          size = size,
          shape = 21,
          fill = "white",
          stroke = 1.5,
          ...
        )
      )
    )
  }


