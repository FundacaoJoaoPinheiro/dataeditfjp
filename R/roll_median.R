#' @title roll_median
#' @description
#' This function computes the roll median of ordered observations.
#'
#' @param variable A variable representing a series of observations.
#' @param fator A scale parameter.
#' @import zoo
#' @importFrom zoo rollmedian
#' @return Vector with TRUE or FALSE if the observation is below 20%(fator) of the roll median of the last 3 years

roll_median <- function(variable, fator){
  res <- fator*zoo::rollmedian(variable, k = 3, fill = NA, align = "right")
  return(res)
}
