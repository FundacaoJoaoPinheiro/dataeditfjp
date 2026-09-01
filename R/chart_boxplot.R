#' @title create a boxplot
#'
#' @description
#' A short description...
#'
#' @param variabe Variable to be analysis
#' @param data data set containing variable
#'
#' @return result

chart_boxplot <- function(variable, data) {

  # Primeiro ano com dados não-NA para essa variável
  ano_inicial <- data |>
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::pull(ano) |>
    min(na.rm = TRUE)

  result <- data |>
    dplyr::filter(ano >= ano_inicial) |>
    ggplot2::ggplot(ggplot2::aes(
      x = as.character(ano),
      y = log(.data[[variable]] + 1),
      fill = as.character(ano)
    )) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_viridis_d(option = "G", direction = -1) +
    ggplot2::labs(
      y = paste0("Valores de ", variable, " em escala log")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

  return(result)
}
