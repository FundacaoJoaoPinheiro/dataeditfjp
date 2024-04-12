#' Outlier data filter
#'
#' @param variable
#'
#' @return resul
#' @export
hampel_filter <- function(variable){
  limite_inf = stats::median(variable, na.rm = T) - 3 * stats::mad(variable, constant = 1, na.rm = T)
  limite_sup = stats::median(variable, na.rm = T) + 3 * stats::mad(variable, constant = 1, na.rm = T)
  result = (variable < limite_inf | variable > limite_sup)
  return(result)
}
