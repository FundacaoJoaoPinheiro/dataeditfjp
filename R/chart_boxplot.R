#' @title create a boxplot
#'
#' @description
#' A short description...
#'
#' @param variabe
#' @param data
#' @param ano_base_alter year start data
#'
#' @return result

chart_boxplot <- function(variable, data, ano_base_alter){
  result <- data  |>
    dplyr::filter(
      dplyr::case_when(
        variable %in% names(ano_base_alter) ~ ANO >= ano_base_alter[variable],
        TRUE ~ ANO >= min(ANO, na.rm = T)) ) |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(ANO), y = log(.data[[variable]] + 1), fill = as.character(ANO))) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_viridis_d(option = "G", direction = -1) + # G: mako
    ggplot2::labs(
      y = paste0("Escala logarítmica de ", variable)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  return(result)
}
