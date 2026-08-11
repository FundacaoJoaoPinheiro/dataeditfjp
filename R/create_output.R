#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param data_path Path to data set
#' @param mydir Output directory where are the files
#' @param lista_cidades List of cities codes with 7 digits. If NULL, will run with 6 cities in the metropolitan area
#' @return Report
#' @export

create_output <- function(mydir=getwd(),
                          data_path=NULL,
                          lista_cidades=NULL
                          ){
  knitmydir = mydir
  if(is.null(lista_cidades)){
    lista_cidades <- c("3106200","3118601","3106705","3157807","3156700","3144805")
  }

  # Prompt para o usuario selecionar a base de dados
  if(is.null(data_path)){
    path_data <- file.choose()
    }else{
      path_data <- data_path
    }
  path_data <- split(path_data,f = path_data)
  noun <- sub(".*\\\\", "", names(path_data))
  message("Verificando colunas do arquivo ",noun)
  noun <- sub("\\.[^.]+$", "", noun)
  names(path_data) <- noun
  colunas_arquivo <- names(head(openxlsx::read.xlsx(path_data[[1]]),1) )
  colunas <- c("IBGE7","ANO","IBGE6","CHAVE")
  nome_colunas <- c("ano","codigo_municipio","indicador","valor")



  `%!in%` <- Negate(`%in%`)
  if(sum(colunas %in% colunas_arquivo)!=0){
    indicadores <- colunas_arquivo[ !(colunas_arquivo %in% colunas) ]
    indicadores_corrigidos <- conferir_nomes_indicadores(indicadores)
    data <- openxlsx::read.xlsx(path_data[[1]])|>
      dplyr::select( -dplyr::any_of(c("IBGE6","CHAVE")))

    posicoes <- match(indicadores, names(data))
    names(data)[posicoes] <- indicadores_corrigidos
    message("Formato valido!\nProcessando...")
    data <- data |>
      dplyr::select( ANO, IBGE7, where(is.numeric)) |>
      tidyr::pivot_longer(cols = !c(ANO, IBGE7), names_to = "indicador", values_to = "valor")

  } else if (sum(nome_colunas %in% colunas_arquivo)!= 0) {
    message("Formato valido!\nProcessando...")
    data <- openxlsx::read.xlsx(path_data[[1]])
  } else {
    message("Formato invalido!\nVerifique o nome das colunas!")
  }

  #caminho para o relatorio
  path_list <- system.file("rmd", "report.Rmd", package = "dataeditfjp")

  rmarkdown::render(input = path_list,
                    knit_root_dir = knitmydir,
                    output_dir = mydir,
                    params = list(data = data,
                                  shown_cities = lista_cidades)
                    )

}

