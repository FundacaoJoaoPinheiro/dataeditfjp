#' @title Diference in percentual
#' @description
#' Calculate the difference between the percentage of municipal participation in the year and the participation in previous year
#'
#' @param df
#' @param variable
#'
#' @return Data frame containing the difference
#' @export

difpercentual <- function(df, variable){
  df %>%
    dplyr::group_by(ANO) %>%
    dplyr::mutate(total = sum({{variable}}, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(IBGE7) %>%
    dplyr::summarise(ANO = ANO,
                     {{variable}} := {{variable}},
                     part = {{variable}}/total,
                     part_lag = dplyr::lag(part, n = 1),
                     dif_prop = part - part_lag,
                     .groups = "drop")
}
