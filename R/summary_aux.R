#' @title summary_aux
#' @description
#' A support function used to summaries rules in data frame.
#'
#' @param df A list with rules applied to variables.
#' @param regra The name of rule to filter data in df.
#' @param variables A vector with names of variables/indicators.
#'
#' @returns A data frame with summaries of results.

summary_aux <- function(df,regra,variables){
  df <- names(df)|>
    sapply(function(i){
      res <- df[[i]] |>
        dplyr::select(ano,codigo_municipio,dplyr::starts_with(regra)) |>
        tidyr::pivot_longer(cols = !c(ano, codigo_municipio), names_to = "Regra", values_to = "resultado")
      return(res)
    }, simplify = FALSE, USE.NAMES = TRUE)
  df <- do.call(rbind,df)
  rownames(df) <- NULL
  df <- df|>
    dplyr::mutate(Regra = gsub(pattern = paste0(regra,"_"),replacement = "",x = Regra)) |>
    dplyr::group_by(ano, Regra) |>
    dplyr::summarise(
      Total        = dplyr::n(),
      Validada     = sum(!resultado, na.rm = TRUE),
      Suspeita     = sum(resultado, na.rm = TRUE),
      Ausente      = sum(is.na(resultado), na.rm = T),
      `% Validada` = round((Validada / Total) * 100, 2),
      `% Suspeita` = round((Suspeita / (Total - Ausente) )* 100, 2),
      `% Ausente` = round((Ausente / Total) * 100, 2)
    ) |>
    dplyr::ungroup()
  return(df)
}
