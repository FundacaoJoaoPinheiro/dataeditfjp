#' @title create a boxplot
#'
#' @description
#' A short description...
#'
#' @param variabe Variable to be analysis
#' @param data data set containing variable
#' @param ano_base_alter year start data
#'
#' @return result

chart_boxplot <- function(variable, data, ano_base_alter){
  result <- data  |>
    dplyr::filter(
      dplyr::case_when(
        variable %in% names(ano_base_alter) ~ ano >= ano_base_alter[variable],
        TRUE ~ ano >= min(ano, na.rm = T)) ) |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(ano), y = log(.data[[variable]] + 1), fill = as.character(ano))) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_viridis_d(option = "G", direction = -1) + # G: mako
    ggplot2::labs(
      y = paste0("Valores de ", variable, " em escala log")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )


  return(result)
}
