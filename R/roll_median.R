#' @title roll_median
#' @description
#' This function computes the roll median of ordered observations.
#'
#' @param variable A variable representing a series of observations.
#' @param fator A scale parameter.
#'
#' @import zoo
#' @importFrom zoo rollmedian
#' @return Vector with TRUE or FALSE

roll_median <- function(variable, fator){
  res <- dplyr::if_else(variable > (fator * zoo::rollmedian(variable, k = 3, fill = NA, align = "right")), F, T)
  return(res)
}
