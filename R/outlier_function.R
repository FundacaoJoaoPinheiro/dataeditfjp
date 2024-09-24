#' @title Detect outlier
#' @description
#' Detect outlier as well as dataMaid::identifyOutliers()
#'
#' @param variable quantitative variable
#' @param a
#' @param b
#' @importFrom robustbase mc
#' @import robustbase
#' @return Vector with TRUE or FALSE

outlier_function <- function(variable, a=-4,b=3){
  q1 <- stats::quantile(variable, probs = 0.25, na.rm = T, names = F)
  q3 <- stats::quantile(variable, probs = 0.75, na.rm = T, names = F)
  iqr <- stats::IQR(variable, na.rm = T)
  mc <- robustbase::mc(variable, na.rm = T)
  inf <- q1 - 1.5*exp(a*mc)*iqr
  sup <- q3 + 1.5*exp(b*mc)*iqr

  check <- ifelse(variable < inf | variable > sup, T, F)

  return(check)
}
