#' @title Chart Line
#'
#' @param data data set
#' @param variable variable to plot
#'
#' @returns Return a graph

chart_line <- function(data, variable){
  result <- data |>
    dplyr::filter(codigo_municipio == "3106200") |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(ano), y = .data[[variable]] )) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(ggplot2::aes(group = 1)) +
    ggplot2::labs(
      y = paste0("Valores de ", variable),
      title = "Belo Horizonte"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1)
    )


  return(result)
}
