#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#' @param mydir Output directory
#' @param knitmydir output knit
#' @param categorical_variable List of qualitative variables
#' @param deleted_variable List of deleted variables
#' @return Report
#' @export

create_output <- function(mydir=getwd(),
                          knitmydir = getwd(),
                          path_file_rules,
                          categorical_variable,
                          deleted_variable,
                          begin_years,
                          deleted_years = c("NA" = NA_integer_)
                          ){

  # Prompt para o usuario selecionar a base de dados
  path_data <- file.choose()
  path_file_rules <- file.choose()
  # Obter o prefixo das variaveis (identificar a dimensão)
  prefix <- substr(rev(colnames(openxlsx::read.xlsx(path_data, rows = 1)))[1], 1, 2)
  # Opcoes de areas
  area <- c("MA", "AS", "EA", "SE", "HS", "SP", "DA", "EO", "GP", "CA", "EL", "FS")

  if(prefix %in% area){
    path_list <- system.file("rmd", "report.Rmd", package = "dataeditfjp")
  } else {
    message("Erro no nome dos indicadore")
  }

  rmarkdown::render(input = path_list,
                    knit_root_dir = knitmydir,
                    output_dir = mydir,
                    params = list(data = path_data,
                                  file_rules = path_file_rules,
                                  #categorical_variable = categorical_variable,
                                  #deleted_variable = deleted_variable,
                                  begin_years = begin_years,
                                  deleted_years = deleted_years))

}

