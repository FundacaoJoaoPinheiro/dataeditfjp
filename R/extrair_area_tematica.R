#' @title Extrair Área Temática
#'
#' @description Extrai a área temática de um código de indicador com base em seu
#'   prefixo. Os prefixos conhecidos são: SE (Saúde), PO (Demografia),
#'   SP (Segurança) e MA (Meio Ambiente).
#'
#' @param indicador Character. Código do indicador (ex: "PO_DENSP", "SE_MORT1",
#'   "SP_HOMIC", "MA_CO2").
#'
#' @returns Character string com o nome da área temática correspondente ao
#'   prefixo do indicador. Retorna "Área Temática" caso nenhum prefixo
#'   conhecido seja encontrado.


extrair_area_tematica <- function(indicador) {

  possiveis_areas_tematicas <- list(
    SE = "DIMENSÃO SAÚDE",
    PO = "DIMENSÃO POPULAÇÃO",
    SP = "DIMENSÃO SEGURANÇA PÚBLICA",
    MA = "DIMENSÃO MEIO AMBIENTE",
    AS = "DIMENSÃO ASSISTENCIA SOCIAL",
    GP = "DIMENSÃO GESTÃO PÚBLICA",
    CA = "DIMENSÃO CULTURA",
    DA = "DIMENSÃO DEMOGRAFIA",
    EA = "DIMENSÃO ECONOMIA",
    EC = "DIMENSÃO ECONOMIA CRIATIVA",
    EL = "DIMENSÃO ESPORTE LAZER",
    EO = "DIMENSÃO EDUCAÇÃO",
    FS = "DIMENSÃO FINANÇAS PÚBLICAS",
    SN = "DIMENSÃO SEGURANÇA ALIMENTAR"
  )

  # Percorre as áreas e verifica se o prefixo está no código do indicador
  encontrados <- names(possiveis_areas_tematicas) |>
    sapply(function(prefixo) {
    grepl(prefixo, indicador)
  })

  # Se encontrou algum prefixo, retorna a primeira área correspondente
  if (any(encontrados)) {
    return(possiveis_areas_tematicas[[names(encontrados)[encontrados][1]]])
  }

  # Se não encontrar nenhum prefixo, retorna genérico
  return("Área Temática")
}
