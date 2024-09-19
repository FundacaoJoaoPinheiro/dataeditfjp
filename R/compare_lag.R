#' @title Compare values
#' @description
#' Compare the actual value with the before
#'
#' @param variable
#' @importFrom dplyr lag
#' @import dplyr
#' @return Vector with TRUE or FALSE

compare_lag <- function(var){
  return(var >= dplyr::lag(var, n = 1))
}
