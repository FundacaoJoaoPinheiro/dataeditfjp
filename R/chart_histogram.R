#' Title
#'
#' @param variable
#' @param data
#'
#' @return
#' @export

chart_histogram <- function(variable, data){
  result <- data |>
    dplyr::mutate(ANO = as.numeric(ANO)) |>
    ggplot2::ggplot(ggplot2::aes(x = log(.data[[variable]] + 1), fill = ANO)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_fill_viridis_c(option = "mako", direction = -1) + # G: mako
    ggplot2::labs(
      y = paste0("Escala logarítmica de ", variable)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title      = ggplot2::element_blank(),
      axis.ticks      = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(~ANO, ncol = 4)

  return(result)
}
