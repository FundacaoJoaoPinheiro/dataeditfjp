#' @title Compare values
#' @description
#' Compare the actual value with the before
#'
#' @param var
#'
#' @return Vector with TRUE or FALSE
#' @export

compare_lag <- function(var){
  return(var >= dplyr::lag(var, n = 1))
}

#' @title Diference in percentual
#' @description
#' Calculate the difference between the percentage of municipal participation in the year and the participation in previous year
#'
#' @param df
#' @param var
#'
#' @return Data frame containing the difference
#' @export
difpercentual <- function(df, var){
  df |>
    dplyr::group_by(ANO) |>
    dplyr::mutate(
      total = sum({{var}}, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(IBGE7) |>
    dplyr::summarise(
      ANO      = ANO,
      {{var}}  := {{var}},
      part     = {{var}} / total,
      part_lag = dplyr::lag(part, n = 1),
      dif_prop = part - part_lag,
      .groups  = "drop"
    )
}

#' @title Outlier data filter
#' @description
#' A short description...
#'
#' @param var
#'
#' @return result
#' @export
hampel_filter <- function(var){
  limite_inf <- stats::median(var, na.rm = T) - 3 * stats::mad(var, constant = 1, na.rm = T)
  limite_sup <- stats::median(var, na.rm = T) + 3 * stats::mad(var, constant = 1, na.rm = T)
  result <- (var < limite_inf | var > limite_sup)
  return(result)
}

#' @title Detect outlier
#' @description
#' Detect outlier as well as dataMaid::identifyOutliers()
#'
#' @param var quantitative variable
#' @param a
#' @param b
#' @importFrom robustbase mc
#' @import robustbase
#' @return Vector with TRUE or FALSE
#' @export
outlier_detection <- function(var, a = -4, b = 3){
  q1 <- quantile(var, probs = 0.25, na.rm = T, names = F)
  q3 <- quantile(var, probs = 0.75, na.rm = T, names = F)
  iqr <- IQR(var, na.rm = T)
  mc <- robustbase::mc(var, na.rm = T)
  inf <- q1 - 1.5 * exp(a * mc) * iqr
  sup <- q3 + 1.5 * exp(b * mc) * iqr

  check <- ifelse(var < inf | var > sup, T, F)

  return(check)
}

compare_first_dif <- function(var){
  dif <- abs(round(c(NA_real_, diff(var)), 2))
  dif_max <- max(lag(dif), na.rm = T)
  check <- ifelse(dif > dif_max, T, F)

  return(check)
}

## Build descriptive table
table_descriptive <- function(var, df = data){
  result <-
    df |>
    dplyr::group_by(ANO) |>
    dplyr::summarise(
      N        = dplyr::n(),
      `Mínimo` = dplyr::na_if(min(.data[[var]], na.rm = T), Inf),
      `Média`  = mean(.data[[var]], na.rm = T),
      Mediana  = median(.data[[var]], na.rm = T),
      `Máximo` = dplyr::na_if(max(.data[[var]], na.rm = T), -Inf),
      `D. P.`  = sd(.data[[var]], na.rm = T),
      `C. V.`  = `D. P.` / `Média`,
      Zero     = sum(.data[[var]] == 0, na.rm = T),
      Missing  = sum(is.na(.data[[var]]))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ANO = as.character(ANO)
    ) |>
    dplyr::rename(
      Ano = ANO
    )

  return(result)
}

## Build quantile table
table_quantile <- function(var, df = data){
  result <-
    df |>
    dplyr::group_by(ANO) |>
    dplyr::summarise(
      valor = quantile(.data[[var]], probs = c(.25, .75), na.rm = T)
    ) |>
    dplyr::mutate(
      quantil = c("25%", "75%")
    ) |>
    tidyr::pivot_wider(id_cols = ANO, values_from = valor, names_from = quantil) |>
    dplyr::mutate(
      ANO = as.character(ANO)
    ) |>
    dplyr::rename(
      Ano = ANO
    )

  return(result)
}

## Build boxplot chart
chart_boxplot <- function(var, df = data){
  result <-
    df |>
    dplyr::mutate(
      ANO = as.character(ANO)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = ANO, y = log(.data[[var]] + 1), fill = ANO)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_viridis_d(option = "G", direction = -1) + # G: mako
    ggplot2::labs(
      y = paste0("Escala logarítmica de ", var)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )

  return(result)
}

## Build histogram chart
chart_histogram <- function(var, df = data){
  result <-
    df |>
    ggplot2::ggplot(ggplot2::aes(x = log(.data[[var]] + 1), fill = ANO)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_fill_viridis_c(option = "G", direction = -1) + # G: mako
    ggplot2::labs(
      y = paste0("Escala logarítmica de ", var)
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
