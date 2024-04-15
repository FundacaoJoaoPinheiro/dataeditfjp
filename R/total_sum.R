#' @title Calculate the total of variable to compare
#' @description
#' A short description...
#'
#' @param variable Quantitative variable
#' @param total Total value to consider as result
#'
#' @return Vector TRUE or FALSE
#' @export
total_sum <- function(variable, total = 100){
  res = ifelse(sum(variable, na.rm = TRUE) == total, TRUE, FALSE)
  return(res)
}
