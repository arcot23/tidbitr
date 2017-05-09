
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
theme_cuckoo <- function(font_family = "Tahoma", base_size = 10, legend_position = c(0.01,0.99))
{
  grid_major_color <- "whitesmoke"
  grid_minor_color <- "snow"
  strip_text_color <- "white"
  axis_line_color <- "gray"
  #ggplot2::theme_bw(base_size = base_size, font_family = font_family) %+replace%
  theme(
    plot.margin =      unit(c(1,1,1,1),"mm")
    , plot.background = element_rect(fill=NA, color=NA, alp)
    , plot.title = element_text(family="Arial Black", size=base_size, hjust = 0)
    , plot.subtitle=element_text(family=font_family, size=base_size)
    , plot.caption=element_text(family=font_family, size=base_size-2)
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
    , strip.background = element_rect(fill = "Gray", color =NA)
    , axis.title = element_text(family=font_family, size=base_size-1)
    , axis.text = element_text(family=font_family, size=base_size)
    , axis.text.x = element_text(hjust = 1, vjust = 1)
    , axis.ticks = element_line(color=NA)
    , axis.line.x = element_line(color=axis_line_color, size = .2)
    , axis.line.y = element_line(color=NA)
  )
}

#' @title Label a ggplot
#' @description A labeler to add title, subtitle, axis titles and footnote to a ggplot.
#'
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param source Label for source that will appear as a footnote.
#' @param dated Label for date that will appear as a footnote.
#' @return Returns the label
#'
#' @examples
#' census %>%
#'   ggplot(aes(x = State, fill = State)) +
#'   geom_bar(stat = "count") +
#'   coord_flip() +
#'   labs_cuckoo(title = "County Frequency by State", subtitle = NULL, xlab = "State") +
#'   theme_cuckoo(font_family = "Tahoma",legend_position = "none")
labs_cuckoo <- function(xlab = "value", ylab = "freq", title = "title", subtitle = paste("Dated", Sys.Date()), source = NA, dated = Sys.Date())
{
  labs(
    x = xlab,
    y = ylab,
    title = title,
    subtitle= subtitle,
    caption=paste0("Source: ", source , "\n" , dated)
  )
}

