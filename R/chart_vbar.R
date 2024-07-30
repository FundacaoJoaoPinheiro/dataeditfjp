#' @title create a vertical barplot
#'
#' @param df
#'
#' @return
#' @export

chart_vbar <- function(df){
  result <-
    df |>
    dplyr::group_by(Regra) |>
    dplyr::summarise(
      Validada = round(sum(Validada) / sum(Total) * 100, 2),
      Suspeita = round(sum(Suspeita) / sum(Total) * 100, 2),
      Ausente  = round(sum(Ausente) / sum(Total) * 100, 2)
    ) |>
    dplyr::mutate(Regra = stringr::str_remove(Regra, "DF_[[:upper:]]{2}_[[:alnum:]]+_")) |>
    tidyr::pivot_longer(cols = !Regra, names_to = "categoria", values_to = "pct") |>
    ggplot2::ggplot(ggplot2::aes(x = pct, y = Regra, fill = categoria)) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(scale = 1, big.mark = ".", decimal.mark = ",")
    ) +
    ggplot2::labs(
      title = "Percentual geral de classificação das observações por regra",
      x     = NULL,
      y     = "Regra\n",
      fill  = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top"
    )
  return(print(result))
}
