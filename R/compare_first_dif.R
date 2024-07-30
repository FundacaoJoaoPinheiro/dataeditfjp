#' @title Compare the first diference
#' @description
#' A short description...
#'
#' @param var
#' @importFrom dplyr lag
#' @import dplyr
#' @return check
#' @export

compare_first_dif <- function(var){
  dif <- abs(round(c(NA_real_, diff(var)), 2))
  dif_max <- max(dplyr::lag(dif), na.rm = T)
  check <- ifelse(dif > dif_max, TRUE, FALSE)

  return(check)
}
