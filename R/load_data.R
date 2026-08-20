#' @title load_data
#' @description
#' Loading data in .xlsx, .csv, .rds or txt.
#'
#' @param caminho File path to be uploaded.
#' @param aba Sheet name or sheet number (only for .xlsx). Default is 1.
#' @param sep Column delimited (only .csv and .txt). Default is ",". For .txt files, use `\t` if the file is tab-delimited.
#' @param encoding Encoding of the .csv or .txt file. Default is "UTF-8".
#' @returns A data.frame (or tibble) with the loaded data
#' @importFrom readr locale
#' @importFrom tools file_ext
#' @importFrom readr read_delim

load_data <- function(caminho, aba = 1, sep = ",", encoding = "UTF-8") {

  if (!file.exists(caminho)) {
    stop(paste("Arquivo não encontrado:", caminho))
  }

  extensao <- tolower(tools::file_ext(caminho))

  dados <- switch(
    extensao,

    "xlsx" = {
      message("Carregando arquivo Excel (.xlsx)...")
      openxlsx::read.xlsx(caminho, sheet = aba)
    },

    "csv" = {
      message("Carregando arquivo CSV (.csv)...")
      readr::read_delim(
        caminho,
        delim = sep,
        locale = readr::locale(encoding = encoding),
        show_col_types = FALSE
      )
    },

    "rds" = {
      message("Carregando arquivo RDS (.rds)...")
      readRDS(caminho)
    },

    "txt" = {
      message("Carregando arquivo de texto (.txt)...")
      readr::read_delim(
        caminho,
        delim = sep,
        locale = readr::locale(encoding = encoding),
        show_col_types = FALSE
      )
    },

    # Caso a extensão não seja reconhecida
    stop(paste0(
      "Formato não suportado: .", extensao,
      "\nFormatos aceitos: .xlsx, .csv, .rds, .txt"
    ))
  )

  message(paste0(
    "Dados carregados com sucesso! (",
    nrow(dados), " linhas x ", ncol(dados), " colunas)"
  ))

  return(dados)
}
