#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#' @param mydir Output directory
#' @param categorical_variable List of qualitative variables
#' @param deleted_variable List of deleted variables
#' @return Report
#' @export

create_output <- function(mydir=getwd()#,
                          # path_file_rules,
                          ){
  knitmydir = mydir#,
  # Prompt para o usuario selecionar a base de dados
  path_data <- file.choose()
  #path_file_rules <- file.choose()
  path_data <- split(path_data,f = path_data)

  noun <- sub(".*\\\\", "", names(path_data))
  message("Verificando colunas do arquivo ",noun)
  noun <- sub("\\.[^.]+$", "", noun)
  names(path_data) <- noun
  colunas_arquivo <- names(head(openxlsx::read.xlsx(path_data[[1]]),1) )
  colunas <- c("IBGE7","ANO")
  nome_colunas <- c("ano","codigo_municipio","indicador","valor")


  if(sum(colunas %in% colunas_arquivo)!=0){
    message("Formato valido!\nProcessando...")
    path_list <- system.file("rmd", "report.Rmd", package = "dataeditfjp")
    data <- openxlsx::read.xlsx(path_data[[1]])|>
      dplyr::select( ANO, IBGE7,-CHAVE,-IBGE6 & where(is.numeric))|>
      tidyr::pivot_longer(cols = !c(ANO, IBGE7), names_to = "indicador", values_to = "valor")

  } else if (sum(nome_colunas %in% colunas_arquivo)!= 0) {
    message("Formato valido!\nProcessando...")
    path_list <- system.file("rmd", "report.Rmd", package = "dataeditfjp")
    data <- openxlsx::read.xlsx(path_data[[1]])
  } else {
    message("Formato invalido!\nVerifique o nome das colunas!")
    }


  rmarkdown::render(input = path_list,
                    knit_root_dir = knitmydir,
                    output_dir = mydir,
                    params = list(data = data#,
                                  #file_rules = path_file_rules,
                                  ))

}

