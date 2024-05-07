#' @title Quantile table
#' @description
#' A short description...
#'
#'
#' @param variable
#' @param data
#'
#' @return
#' @export

table_quantile <- function(variable, data){
  result <- data  %>%
    dplyr::group_by(ANO)  %>%
    dplyr::summarise(value = stats::quantile(.data[[variable]], probs = c(.25, .75), na.rm = T),
                     .groups  = "drop")  %>%
    dplyr::mutate(quantil = rep(c("25%", "75%"),length(unique(ANO))) ) %>%
    tidyr::pivot_wider(id_cols = ANO, values_from = value, names_from = quantil)
  return(result)
}
