#' @title summary_aux
#' @description
#' A support function used to summaries rules in data frame.
#'
#' @param df A list with rules applied to variables.
#' @param regra The name of rule to filter data in df.
#' @param variables A vector with names of variables/indicators.
#'
#' @returns A data frame with summaries of results.

data_valid <- function(df,regras,variables,cidades){
  df <- names(df) |>
    sapply(function(i){
      res_aux <- df[[i]] |>
        dplyr::select(ano,codigo_municipio, dplyr::starts_with(paste0(regras,"_")) ) |>
        tidyr::pivot_longer(cols = !c(ano,codigo_municipio), names_to = "Regra", values_to = "obs_suspeita") |>
        dplyr::mutate(Regra = gsub(pattern = paste0(regras,"_"), replacement = "",x = Regra))

      res <- df[[i]] |>
        dplyr::select(ano,codigo_municipio, variables) |>
        tidyr::pivot_longer(cols = !c(ano,codigo_municipio), names_to = "Regra", values_to = "resultado") |>
        dplyr::left_join(res_aux) |>
        dplyr::filter(obs_suspeita)

      res <- res |>
        dplyr::left_join(cidades,
                         by = dplyr::join_by(codigo_municipio==codigo_ibge)) |>
        dplyr::select(ano,codigo_municipio, nome_municipio, indicador=Regra, resultado)

    }, USE.NAMES = TRUE, simplify = FALSE)

}
