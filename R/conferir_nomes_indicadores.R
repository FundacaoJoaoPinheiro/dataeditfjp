#' @title conferir_nomes_indicadores
#' @description
#' nome_colunas deve conter todos os indicadores ( nemso : "CHAVE","IBGE6","IBGE7","ANO").
#' A funcao conferir_nomes_indicadores retorna um vetor com os nomes das colunas corrigidas.
#'
#' @param nome_colunas Names of indicators
#'
#' @returns A vector with correct indicators names.

conferir_nomes_indicadores <- function(nome_colunas) {
  corrigidas <- rep(0, length(nome_colunas))

  repeat {
    if (all(corrigidas == 1)) break

    for (i in seq_along(nome_colunas)) {
      if (corrigidas[i] == 0) {

        nome_atual <- nome_colunas[i]
        derivado   <- paste0("mediana_movel_max_", nome_atual)
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

          novo_nome <- ""
          valido <- FALSE

          while (!valido) {
            novo_nome <- readline(prompt = "Adicione o novo nome para esta coluna: ")
            novo_nome <- trimws(novo_nome)

            if (!nzchar(novo_nome)) {
              cat("[!] O nome nao pode ser vazio. Tente novamente.\n")
            } else if (novo_nome %in% nome_colunas[-i]) {
              cat("[!] Este nome ja existe em outra coluna. Escolha um nome unico.\n")
            } else {
              valido <- TRUE
            }
          }

          nome_colunas[i] <- novo_nome
        } else {
          corrigidas[i] <- 1
        }
      }
    }
  }

  return(nome_colunas)
}
