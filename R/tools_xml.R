library(xml2)
#' @title Gets all XML node text
#' @description Parses through all XML end nodes and returns the text of those nodes in a tibble.
#'
#' @param x XML document.
#' @return Returns xpath to a node, node name and text value in a tibble.
#'
#' @examples
#' get_all_node_text(xml2::read_xml("https://www.w3schools.com/xml/simple.xml")) %>%
#'   View()
get_all_node_text <- function(x)
{
  all_nodes <- xml_find_all(x, ".//*")
  df <- data.frame()
  for (i in 1:length(all_nodes))
  {
    if (length(xml_children(all_nodes [i])) == 0)
    {
      path <- xml_path(all_nodes [i])
      name <- xml_name(all_nodes [i])
      txt <- xml_text(all_nodes [i])
      df <- rbind(df, cbind(xpath = path, node = name, value = txt))
    }
  }
  tibble::as_data_frame(df)
}
