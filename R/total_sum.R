#' Calculate the total of variable to compare
#'
#' @param variable
#' @param total
#'
#' @return
#' @export
total_sum <- function(variable, total = 100){
  res = (sum(variable, na.rm = TRUE) == total)
  return(res)
}
