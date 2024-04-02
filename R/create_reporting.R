create_reporting <- function(path_data){

  path_report <- "R/report.Rmd"

  return(
    rmarkdown::render(input = path_report, params = list(data = path_data))

    )
}
