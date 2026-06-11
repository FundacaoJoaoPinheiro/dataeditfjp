#' descriptiveanalisys
#' @description function for calculate the descriptive statistics of the data
#' @param variable Variable to be analysis
#' @param data data set containing variable
#'
#' @return statistics of the data
#' @export
#' @importFrom stats median sd
#'

descriptiveanalisys <- function(variable, data){
  res <- data |>
    dplyr::group_by(ano) |>
    dplyr::summarise(
      N = dplyr::n(),
      `Mínimo` = dplyr::na_if(min(.data[[variable]], na.rm = TRUE),Inf),
      `Média` = mean(.data[[variable]], na.rm = TRUE),
      Mediana = median(.data[[variable]], na.rm = TRUE),
      `Máximo` = dplyr::na_if(max(.data[[variable]], na.rm = TRUE),-Inf),
      `Desvio padrão` = sd(.data[[variable]], na.rm = TRUE),
      `Coef. Variação` = sd(.data[[variable]], na.rm = TRUE)/mean(.data[[variable]], na.rm = TRUE),
      Zeros = sum(.data[[variable]] == 0, na.rm = TRUE),
      Ausentes = sum(is.na(.data[[variable]]))
    )|>
    dplyr::ungroup()
  return(res)
}
