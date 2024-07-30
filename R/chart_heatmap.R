#' @title create a heatmap chart
#' @description
#' A short description...
#'
#' @param df
#'
#' @return
#' @export


chart_heatmap <- function(df){
  result <-
    df |>
    dplyr::mutate(
      Ano   = as.character(Ano),
      Regra = stringr::str_remove(Regra, "DF_[[:upper:]]{2}_[[:alnum:]]+_")
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = Regra, y = Ano, fill = Suspeita)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = Suspeita), size = 3) +
    ggplot2::scale_fill_distiller(palette = "OrRd", direction = 1) +
    ggplot2::labs(
      title = "Total de observações suspeitas por regra e ano",
      x     = "\nRegra",
      y     = "Ano\n"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "None",
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1)
    )
  return(print(result))
}
