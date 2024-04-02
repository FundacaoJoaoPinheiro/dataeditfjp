create_reporting <- function(path_data){

  #path_data <- "C:/Users/edcam/Downloads/IMRS_BASE_ESPORTE_2000-2022.xlsx"
  path_report <- "R/report.Rmd"

  return(
    rmarkdown::render(input = path_report,  params = list(data = path_data), output_dir = "../")
    )
}
