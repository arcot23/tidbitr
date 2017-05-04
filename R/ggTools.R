library(tidyverse)
library(extrafont)

marco.fill <- function(color_palette = default_palette) {
  ggplot2::scale_fill_brewer(palette = color_palette)
  #scale_fill_manual(values =  colorRampPalette(brewer.pal(9, color_palette))(100))
}

marco.color <- function(color_palette = default_palette) {
  scale_colour_brewer(palette = color_palette)
  #ggplot2::scale_colour_manual(values =  colorRampPalette(brewer.pal(9, color_palette))(100))
}

marco.draw_titles <- function(xlab = "value", ylab = "freq", title, subtitle = paste("Dated", Sys.Date()), source = NA, dated = Sys.Date())
{
  labs(
    x = xlab,
    y = ylab,
    title= title,
    subtitle=subtitle,
    caption=paste0("Source: ", source , "\n" , dated)
  )
}

marco.draw_label <- function(xpoint, ypoint, lbl)
{
  geom_label(data=data.frame(), hjust=1, vjust=1, nudge_x=-0.5, label.size=0, size=3,
             aes_string(x=xpoint, y=ypoint), label=lbl,
             family="Franklin Gothic Book", color="white", fill = "RED")
}


marco.gpearl <- function(aes_x, aes_y, color = "red", size = 2, shape = 21, fill = "white"){
  geom_point(aes_string(x=aes_x, y=aes_y), color=color, size = size, shape = shape, fill = fill)
}

marco.gline <- function(aes_x, aes_y, color = "red", size = 1.5){
  geom_line(aes_string(x=aes_x, y=aes_y), color=color, size = size)
}

marco.theme <- function(base_size = 10, base_family = "Franklin Gothic Book", legend_position = c(0.01,0.99))
{
  grid_major_color <- "whitesmoke"
  grid_minor_color <- "snow"
  strip_text_color <- "white"
  axis_line_color <- "gray"
  #ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
  theme(
    plot.margin =      unit(c(1,1,1,1),"mm")
    , plot.background = element_rect(fill=NA, color=NA, alp)
    , plot.title = element_text(family="Franklin Gothic Heavy", size=base_size, hjust = 0)
    , plot.subtitle=element_text(family=base_family, size=base_size)
    , plot.caption=element_text(family=base_family, size=base_size-2)
    , panel.grid.minor =  element_line(color = grid_minor_color)
    , panel.grid.major =  element_line(color = grid_major_color)
    , panel.border = element_rect(fill = NA, color = NA)
    , panel.background = element_rect(fill = "white")
    , legend.position = legend_position
    , legend.justification = c(0,1)
    , legend.title = element_text(family=base_family, size=base_size-1)
    , legend.background = element_rect(fill=alpha("black", 0.05), color=NA, alp, linetype = "dotted")
    , legend.key=element_rect(color=NA, fill =NA)
    , legend.text = element_text(family=base_family, size=base_size-2)
    , strip.text.x = element_text(family=base_family, size=base_size, color = strip_text_color)
    , strip.text.y = element_text(family=base_family, size=base_size, color = strip_text_color)
    , strip.background = element_rect(fill = "Gray", color =NA)
    , axis.title = element_text(family=base_family, size=base_size-1)
    , axis.text = element_text(family=base_family, size=base_size)
    , axis.text.x = element_text(hjust = 1, vjust = 1)
    , axis.ticks = element_line(color=NA)
    , axis.line.x = element_line(color=axis_line_color, size = .2)
    , axis.line.y = element_line(color=NA)
  )
}

marco.world_map <- function(map_title = "World Map", sub_title= paste("Dated", Sys.Date()), border_color = "grey", fill_color = NA)
{
  world_map_data<-map_data("world")
  ggplot() +
    geom_polygon(data=world_map_data, aes(x=long,y=lat,group=group), color = border_color, size = 0.1, fill=fill_color) +
    marco.theme() +
    marco.draw_titles("Longitude", "Latitude", title = map_title, subtitle = sub_title)
}


gg_bar <- function(data, xaxis, yaxis, xlab = "x", ylab = "y", title) {
  #data$xaxis <- data[xaxis]
  #data$yaxis <- data[yaxis]

  ggplot2::ggplot (data , aes_string(x = xaxis, y = yaxis, fill = yaxis)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_text(aes_string(label = yaxis), hjust = 1.5, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::theme(
      legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1), plot.margin =
        unit(c(0,0,0,0),"mm")
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_brewer(palette = default_palette) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(x = xlab, y = ylab)  +
    ggplot2::scale_y_log10(
      labels = scales::comma, breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000, 5000000, 50000000)
    )
}
