#' @title Outlier data filter
#' @description
#' Identifies outliers calculating the upper and lower limits based on three standard deviations.
#'
#' @param variable variable or indicators
#' @return result

outlier_filter <- function(variable){
  limite_inf = mean(variable, na.rm = T) - 3*sd(variable,na.rm = T)
  limite_sup = mean(variable, na.rm = T) + 3*sd(variable,na.rm = T)
  result = ifelse(variable < limite_inf | variable > limite_sup, T, F)
  return(result)
}
