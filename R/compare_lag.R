#' @title Compare values
#' @description
#' Compare the actual value with the before
#'
#' @param variable
#'
#' @return Vector with TRUE or FALSE
#' @export

compare_lag <- function(variable){
  return(variable >= dplyr::lag(variable, n = 1))
}
