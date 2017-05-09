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
