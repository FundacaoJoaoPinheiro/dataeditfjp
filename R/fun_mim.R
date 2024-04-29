
#' @title Calcula o minimo
#'
#' @param x A quantitative variable
#'
#' @return The minimo value
#' @export
#'
#' @examples
fun_min <- function(x){
  res <- min(x, na.rm = T)
  return(res)
  }
