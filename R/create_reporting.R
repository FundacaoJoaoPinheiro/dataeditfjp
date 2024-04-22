#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#'
#' @return Report
#' @export
create_reporting <- function(path_data,path_metadata){

  #path_data <- "C:/Users/edcam/Downloads/IMRS_BASE_MEIO-AMBIENTE_2000-2022.xlsx"
  #path_metadata <-"C:/Users/edcam/Downloads/IMRS_METADADOS_MEIO-AMBIENTE.xlsx"
  path_report <- "R/report.Rmd"

  return(
    rmarkdown::render(input = path_report,
                      params = list(data = path_data,
                                    metadata = path_metadata),
                      output_dir = "../")
    )
}
