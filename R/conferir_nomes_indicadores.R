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

    if (all(corrigidas == 1)) break             #condição de parada:todas corrigidas
    for (i in seq_along(nome_colunas)) {

      if (corrigidas[i] == 0) {

        nome_atual <- nome_colunas[i]
        derivado   <- paste0("mediana_movel_max_", nome_atual)   #concatena

        problemas <- character(0)

        # Prefixo de 2 letras
        if (!grepl("^[A-Za-z]{2}_", nome_atual)) {
          problemas <- c(problemas, "sem prefixo de 2 letras")
        }

        if (length(problemas) > 0) {
          message("\n[ATENÇÃO] Coluna '",
                  nome_atual,
                  "': ",
                  paste(problemas, collapse = " e "))

          novo_nome <- ""
          valido <- FALSE

          while (!valido) {
            novo_nome <- readline(prompt = "Adicione o novo nome para esta coluna: ")
            novo_nome <- trimws(novo_nome)

            # não pode ser vazio (i) ou repetido (ii)
            if (!nzchar(novo_nome)) {
              cat("[!] O nome não pode ser vazio. Tente novamente.\n")
            } else if (novo_nome %in% nome_colunas[-i]) {
              cat("[!] Este nome já existe em outra coluna. Escolha um nome único.\n")
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
