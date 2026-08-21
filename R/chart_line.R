#' @title Chart Line
#'
#' @param data data set
#' @param variable variable to plot
#' @param list_city_aux list of cities
#'
#' @returns Return a graph

chart_line <- function(data, variable, list_city_aux){
  result <- data |>
    dplyr::right_join(list_city_aux)|>
    ggplot2::ggplot(ggplot2::aes(x = ano, y = .data[[variable]], text = c() )) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(ggplot2::aes(group = 1)) +
    ggplot2::labs(
      y = paste0("Valores de ", variable)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 75, vjust = 0.7, hjust = 1, size = 8)
    ) +
  ggplot2::facet_wrap(~nome_municipio,ncol = 2)

  return(result)
}
