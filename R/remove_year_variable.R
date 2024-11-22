#' @title Remove years from variable
#'
#' @param data Dataset
#' @param variable Variable to remove the years
#' @param exclusions_years List of deleted years
#'
#' @return A dataset without years selected

remove_year_variable <- function(data, variable,exclusions_years){
  `%!in%` <- Negate(`%in%`)
  if(variable %in% names(exclusions_years)){
    df <- data |>
      dplyr::filter( ANO %!in% exclusions_years[variable])
  } else {
    df <- data
  }
  return(df)
}
