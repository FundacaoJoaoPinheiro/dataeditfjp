#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#'
#' @return Report
#' @export

create_report <- function(){

  path_list <- c(
    "MA" = system.file("qmd", "ma_report.qmd", package = "dataeditfjp"),
    "AS" = system.file("qmd", "as_report.qmd", package = "dataeditfjp"),
    "EA" = system.file("qmd", "ea_report.qmd", package = "dataeditfjp"),
    "SE" = system.file("qmd", "se_report.qmd", package = "dataeditfjp"),
    "HS" = system.file("qmd", "hs_report.qmd", package = "dataeditfjp"),
    "SP" = system.file("qmd", "sp_report.qmd", package = "dataeditfjp"),
    "DA" = system.file("qmd", "da_report.qmd", package = "dataeditfjp"),
    "EO" = system.file("qmd", "eo_report.qmd", package = "dataeditfjp"),
    "GP" = system.file("qmd", "gp_report.qmd", package = "dataeditfjp"),
    "CA" = system.file("qmd", "ca_report.qmd", package = "dataeditfjp"),
    "EL" = system.file("qmd", "el_report.qmd", package = "dataeditfjp"),
    "FS" = system.file("qmd", "fs_report.qmd", package = "dataeditfjp")
  )


  # Prompt para o usuário selecionar a base de dados
  path_data <- file.choose()

  # Obter o prefixo das variáveis (identificar a dimensão)
  prefix <- substr(rev(colnames(openxlsx::read.xlsx(path_data, rows = 1)))[1], 1, 2)

  return(
    quarto::quarto_render(
      input          = path_list[prefix],
      output_format  = "html",
      quiet          = F,
      execute_params = list(data = path_data),
      #cache          = T
    )
  )
}
