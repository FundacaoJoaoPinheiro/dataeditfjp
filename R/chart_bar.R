#' @title create barplot
#' @description
#' A short description...
#'
#' @param ano
#' @param n
#'
#' @return
#' @export
#'

chart_bar <- function(ano, n){
  result <- data.frame(ano, n) |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(ano), y = as.integer(n))) +
    ggplot2::geom_bar(stat = "identity", fill = "#cccccc") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    ggplot2::labs(
      title = "Observações suspeitas por ano",
      y     = "Total\n"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  return(result)
}
