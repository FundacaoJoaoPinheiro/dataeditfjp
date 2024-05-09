#' Title
#'
#' @param var
#'
#' @return
#' @export

compare_first_dif <- function(var){
  dif <- abs(round(c(NA_real_, diff(var)), 2))
  dif_max <- max(abs(round(c(NA_real_, diff(var)), 2)), na.rm = T)
  check <- ifelse(dif >= dif_max, T, F)

  return(check)
}
