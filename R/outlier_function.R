#' @title Detect outlier
#' @description
#' Detects outliers in historical data, exluding the last observation.
#' @param variable quantitative variable
#' @importFrom dplyr lag
#' @import dplyr
#' @return Vector with TRUE or FALSE

outlier_function <- function(variable){
  q1 <- stats::quantile(variable, probs = 0.25, na.rm = T, names = F)
  q3 <- stats::quantile(variable, probs = 0.75, na.rm = T, names = F)
  iqr <- q3 - q1
  inf <- q1 - 2*iqr
  sup <- q3 + 2*iqr

  check <- ifelse(variable < inf | variable > sup, T, F)

  return(check)
}
