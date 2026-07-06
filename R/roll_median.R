#' @title roll_median
#' @description
#' This function computes the roll median of ordered observations.
#'
#' @param variable A variable representing a series of observations.
#' @param fator A scale parameter.
#' @param defaut If false return the values with TRUE or FALSE if the observation is above 20% (fator) the  roll median of the last 3 years.
#' @import zoo
#' @importFrom zoo rollmedian
#' @return Vector with TRUE or FALSE if the observation is below 20%(fator) of the roll median of the last 3 years

roll_median <- function(variable, fator, defaut = T){
  if(defaut){res <- dplyr::if_else(variable > (fator * zoo::rollmedian(variable, k = 3, fill = NA, align = "right")), F, T)}

  if(defaut==F){res <- dplyr::if_else(variable < (fator * zoo::rollmedian(variable, k = 3, fill = NA, align = "right")), F, T)}
  return(res)
}
