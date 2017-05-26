
#' @title Applies the ggplots theme "cuckoo"
#' @description A theme that can be applied to a ggplot.
#'
#' @param font_family Informs the font family to be used as a base. Defaulted to Tahoma.
#' @param base_size Informs the base font size. Defaulted to size 10.
#' @param legend_position Informs the postion of the legend. Use "none" to hide the legend.
#' @return Returns the theme
#'
#' @examples
#' census %>%
#'   ggplot(aes(x = State, fill = State)) +
#'   geom_bar(stat = "count") +
#'   coord_flip() +
#'   theme_cuckoo(font_family = "Tahoma",legend_position = "none")
theme_cuckoo <- function(font_family = "Arial", base_size = 10, legend_position = c(0.01,0.99), ...)
{
  grid_major_color <- "whitesmoke"
  grid_minor_color <- "snow"
  strip_text_color <- "white"
  axis_line_color <- "gray"
  color <- "darkseagreen3"
  update_geom_defaults(geom = "text", list(family = font_family, size = round(base_size/3.333)))
  update_geom_defaults(geom = "label", list(family = font_family, fill = grid_major_color, size = round(base_size/3.333)))
  update_geom_defaults(geom = "bar", list(size = 2, fill = color))
  update_geom_defaults(geom = "point", list(size = 3, shape = 21, colour = color, fill = grid_major_color))
  update_geom_defaults(geom = "line", list(size = 2, colour = color))
  #ggplot2::theme_bw(base_size = base_size, font_family = font_family) %+replace%
  theme(
    plot.margin =      unit(c(1,1,1,1),"mm")
    , plot.background = element_rect(fill=NA, color=NA, alp)
    , plot.title = element_text(family="Arial Black", face = "bold", size=base_size, hjust = 0)
    , plot.subtitle=element_text(family=font_family, size=base_size)
    , plot.caption=element_text(family=font_family, size=base_size-2, hjust = 0, color = axis_line_color)
    , panel.grid.minor =  element_line(color = grid_minor_color)
    , panel.grid.major =  element_line(color = grid_major_color)
    , panel.border = element_rect(fill = NA, color = NA)
    , panel.background = element_rect(fill = "white")
    , legend.position = legend_position
    , legend.justification = c(0,1)
    , legend.title = element_text(family=font_family, size=base_size-1)
    , legend.background = element_rect(fill=alpha("black", 0.05), color=NA, alp, linetype = "dotted")
    , legend.key=element_rect(color=NA, fill =NA)
    , legend.text = element_text(family=font_family, size=base_size-2)
    , strip.text.x = element_text(family=font_family, size=base_size, color = strip_text_color)
    , strip.text.y = element_text(family=font_family, size=base_size, color = strip_text_color)
    , strip.background = element_rect(fill = axis_line_color, color =NA)
    , axis.title = element_text(family=font_family, size=base_size)
    , axis.text = element_text(family=font_family, size=base_size)
    , axis.text.x = element_text(hjust = 1, vjust = 1)
    , axis.ticks = element_line(color=axis_line_color, size = 1)
    , axis.line.x = element_line(color=axis_line_color, size = 1)
    , axis.line.y = element_line(color=NA)
    , ...
  )
}


#' @title Add text in the plot
#' @description Adds text to the histograms or points.
#'
#' @param lbl Column value to be displayed as label.
#' @param font_family Font family. Defaulted to Tahoma.
#' @param hjust Horizontal justification as Left Justified.
#' @return Returns the text
#'
#' @examples
#' census %>%
#'   ggplot(aes(x = State, color = State)) +
#'   geom_point(stat = "count", size = 8) +
#'   labs_cuckoo(title = "County Frequency by State", subtitle = NULL, xlab = "State") +
#'   text_cuckoo("..count..", color = "black", hjust = 0.5) +
#'   theme_cuckoo(font_family = "Tahoma",legend_position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
text_cuckoo <- function(lbl, font_family = "Tahoma", hjust = 1, ...)
{
  geom_text(
    aes_string(label = lbl),
    size = 3,
    stat = "count",
    family = font_family,
    hjust = hjust,
    vjust = 0.5,
    ...
  )
}
