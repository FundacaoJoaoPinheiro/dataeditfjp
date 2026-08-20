#' @title conferir_indicadores_longo
#' @description
#' nome deve conter todos os indicadores na coluna "indicador".
#' A funcao confere os nomes dos indicadores e aponta qual deve ser corrigido.
#'
#' @param nome_colunas Names of indicators
#'
#' @returns A vector with incorrect indicators names.

conferir_indicadores_longo <- function(nome_colunas) {
  colunas_problematicas <- list()
  for (i in seq_along(nome_colunas)) {

    nome_atual <- nome_colunas[i]

    problemas  <- character(0)

    # Regra 1: prefixo de 2 letras + underscore
    tem_prefixo <- grepl("^[A-Za-z]{2}_", nome_atual)
    if (!tem_prefixo) {
      problemas <- c(problemas, "sem prefixo de 2 letras")
    }

    # Regra 2: sufixo vazio (apenas prefixo, sem nome do indicador)
    if (tem_prefixo) {
      sufixo <- substring(nome_atual, 4)
      if (!nzchar(sufixo)) {
        problemas <- c(problemas, "nome do indicador vazio (apenas prefixo)")
      }
    }

    # Regra 3: underline extra no nome do indicador
    if (tem_prefixo) {
      sufixo <- substring(nome_atual, 4)
      if (nzchar(sufixo) && grepl("_", sufixo)) {
        problemas <- c(problemas, "contem underline extra no nome")
      }
    } else {
      if (grepl("_", nome_atual)) {
        problemas <- c(problemas, "contem underline extra no nome")
      }
    }

    # Regra 4: espaco no nome
    if (grepl(" ", nome_atual)) {
      problemas <- c(problemas, "contem espaco no nome")
    }

    if (length(problemas) > 0) {
      message("\n[ATENCAO] Coluna '",
              nome_atual,
              "': ",
              paste(problemas, collapse = " e "))

      colunas_problematicas <- append(colunas_problematicas, nome_atual)
    }
  }
  return(colunas_problematicas)
}
