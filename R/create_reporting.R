#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#'
#' @return Report
#' @export
create_reporting <- function(path_data,path_metadata, output_path = getwd()){

  # path_data <- "C:/Users/edcam/Downloads/IMRS_BASE_MEIO-AMBIENTE_2000-2022.xlsx"
  # path_metadata <-"C:/Users/edcam/Downloads/IMRS_METADADOS_MEIO-AMBIENTE.xlsx"
  # #path_report <- "R/report.Rmd"


  var_ma <- c("M_AGRO", "M_AIIGENA", "M_BOVINO", "M_CANA" )
  var_sa <- c("B_NCRAS", "B_FCADMEIOSAL", "B_INDCRAS", "B_IDCRASME")



  metadata_var <- openxlsx::read.xlsx(path_metadata) %>%
    dplyr::filter(`DIMENSÃO` != "atlas") %>%
    dplyr::pull(SIGLA)

  if(sum(var_ma %in% metadata_var) != 0){area_tematica = "ma"}
  if(sum(var_sa %in% metadata_var) != 0){area_tematica = "sa"}
  # if(sum(var_ %in% metadata_var) != 0){}
  # if(sum(var_ %in% metadata_var) != 0){}
  # if(sum(var_ %in% metadata_var) != 0){}

  path_report <- system.file("rmd",paste0("report_",area_tematica,".Rmd"),package = "criticaldatafjp")

  return(
    rmarkdown::render(input = path_report,
                      params = list(data = path_data,
                                    metadata = path_metadata),
                      output_dir = output_path)
    )
}
