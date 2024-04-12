#' Outlier data filter
#'
#' @param variable
#'
#' @return
#' @export
hampel_filter <- function(variable){
  limite_inf = median(variable, na.rm = T) - 3 * mad(variable, constant = 1, na.rm = T)
  limite_sup = median(variable, na.rm = T) + 3 * mad(variable, constant = 1, na.rm = T)
  result = (variable < limite_inf | variable > limite_sup)
  return(result)
}
