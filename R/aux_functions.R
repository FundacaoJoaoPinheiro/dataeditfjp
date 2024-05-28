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

## Comparison between the values as first difference
compare_first_dif <- function(var){
  dif <- abs(round(c(NA_real_, diff(var)), 2))
  dif_max <- max(dplyr::lag(dif), na.rm = T)
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
      Ausente  = sum(is.na(.data[[var]]))
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

# Build horizontal bar chart
chart_hbar <- function(ano, n){
  result <-
    data.frame(ano, n) |>
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

  return(print(result))
}

# Build vertical bar chart
chart_vbar <- function(df){
  result <-
    df |>
    dplyr::group_by(Regra) |>
    dplyr::summarise(
      Validada = round(sum(Validada) / sum(Total) * 100, 2),
      Suspeita = round(sum(Suspeita) / sum(Total) * 100, 2),
      Ausente  = round(sum(Ausente) / sum(Total) * 100, 2)
    ) |>
    dplyr::mutate(Regra = stringr::str_remove(Regra, "DF_[[:upper:]]{2}_[[:upper:]]+_")) |>
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

# Build heatmap
chart_heatmap <- function(df){
  result <-
    df |>
    dplyr::mutate(
      Ano   = as.character(Ano),
      Regra = stringr::str_remove(Regra, "DF_[[:upper:]]{2}_[[:upper:]]+_")
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
