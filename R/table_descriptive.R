#' @title create descriptive table
#' @description
#' A short description...
#'
#' @param variable
#' @param data Dataset
#' @return A dataframe with de summary statistics
#' @export

table_descriptive <- function(variable, data){
  res <- data |>
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
