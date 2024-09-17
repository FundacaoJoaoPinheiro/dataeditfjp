#' @title create descriptive table
#' @description
#' A short description...
#'
#' @param variable
#' @param data Dataset
#' @param ano_base_alter start year
#' @return A dataframe with de summary statistics

table_descriptive <- function(variable, data, ano_base_alter){
  res <- data |>
    dplyr::filter(
      dplyr::case_when(
        variable %in% names(ano_base_alter) ~ ANO >= ano_base_alter[variable],
        TRUE ~ ANO >= min(ANO, na.rm = T)) ) |>
    dplyr::group_by(ANO) |>
    dplyr::summarise(
      N = dplyr::n(),
      `Mínimo`=dplyr::na_if(min(.data[[variable]], na.rm = TRUE), Inf),
      `Média`  = mean(.data[[variable]], na.rm = TRUE),
      Mediana  = median(.data[[variable]], na.rm = TRUE),
      `Máximo` = dplyr::na_if(max(.data[[variable]], na.rm = TRUE), -Inf),
      `D. P.`  = sd(.data[[variable]], na.rm = TRUE),
      `C. V.`  = sd(.data[[variable]], na.rm = TRUE)/mean(.data[[variable]], na.rm = TRUE),
      Zero     = sum(.data[[variable]] == 0, na.rm = T),
      Ausentes  = sum(is.na(.data[[variable]])),
      .groups = "drop")

  return(res)
}
