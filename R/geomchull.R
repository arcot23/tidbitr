#' @title Create a polygon plot with boundaries
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_chull(aes(fill = manufacturer, color = manufacturer)) +
#'   theme_cuckoo(legend_position = "right")

geom_chull <-
  function(mapping = NULL,
           data = NULL,
           geom = "polygon",
           position = "identity",
           show.legend = NA,
           inherit.aes = TRUE,
           alpha = 0.1,
           ...) {
    GeomChull <- ggplot2::ggproto(
      "GeomChull",
      ggplot2::Stat,
      compute_group = function(data, scales) {
        data[chull(data$x, data$y), , drop = FALSE]
      },

      required_aes = c("x", "y")
    )

        ggplot2::layer(
      stat = GeomChull,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(alpha = alpha, ...)
    )
  }
