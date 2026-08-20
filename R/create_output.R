#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param data_path Path to data set
#' @param mydir Output directory where are the files
#' @param lista_cidades List of cities codes with 7 digits. If NULL, will run with 6 cities in the metropolitan area.
#' @param fator_mediana Factor applied to the moving median. The result is a threshold with k% above or below the median. Default is 20%.
#' @param regras Selects a set of distributional check/rules. If `regras = NULL`, apply all rules. If the vector is set to `regras = c("outlier_mean", "mediana_movel_min", "max")`, the selected rules are applied. One or more rules may be chosen from the set `c("outlier", "outlier_mean", "mediana_movel_min", "mediana_movel_max", "min", "max")`. This checking mostly refers to the procedure of detecting the outliers or non-standard observations.
#' @return Report
#' @export

create_output <- function(mydir=getwd(),
                          data_path=NULL,
                          lista_cidades=NULL,
                          fator_mediana = 0.20,
                          regras = NULL
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
  data <- load_data(path_data[[1]])
  colunas_arquivo <- names(data)
  colunas <- c("IBGE7","ANO","IBGE6","CHAVE")
  nome_colunas <- c("ano","codigo_municipio","indicador","valor")

  if(sum(colunas %in% colunas_arquivo)!=0){
    indicadores <- colunas_arquivo[ !(colunas_arquivo %in% colunas) ]
    indicadores_corrigidos <- conferir_nomes_indicadores(indicadores)
    data <- data |>
      dplyr::select( -dplyr::any_of(c("IBGE6","CHAVE")))

    posicoes <- match(indicadores, names(data))
    names(data)[posicoes] <- indicadores_corrigidos
    message("Formato valido!\nProcessando...")
    data <- data |>
      dplyr::select( ANO, IBGE7, where(is.numeric)) |>
      tidyr::pivot_longer(cols = !c(ANO, IBGE7), names_to = "indicador", values_to = "valor")

  } else if (sum(nome_colunas %in% colunas_arquivo)!= 0) {
    message("Formato valido!\nProcessando...")
    indicadores <- unique(data$indicador)
    indicadores_problematicos <- conferir_indicadores_longo(indicadores)
    if(length(indicadores_problematicos) != 0){
      message("Nomes dos indicadores com problemas: ", paste(indicadores_problematicos, collapse = " e "), "\n\nCorrija os nomes em sua base!")
      message("Os nomes dos indicadores estão fora do padrão!")
      return(invisible(NULL))
    } else{
      data <- data
    }

  } else {
    message("Formato invalido!\nVerifique o nome das colunas!")
  }

  #caminho para o relatorio
  path_list <- system.file("rmd", "report.Rmd", package = "dataeditfjp")

  rmarkdown::render(input = path_list,
                    knit_root_dir = knitmydir,
                    intermediates_dir = mydir,
                    output_dir = mydir,
                    params = list(data = data,
                                  shown_cities = lista_cidades,
                                  fator = fator_mediana,
                                  distributional_check = regras)
                    )

}

