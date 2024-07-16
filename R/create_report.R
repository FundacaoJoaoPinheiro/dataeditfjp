#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#'
#' @return Report
#' @export

## To-do: criar uma função para mover o relátorio "relatorio.html" do diretório
## \inst\qmd para a raíz do projeto, dado que quarto::quarto_render() não oferece
## esta opção
create_report <- function(){

  # path_list <- c(
  #   "MA" = here::here("inst", "qmd", "ma_report.qmd"),
  #   "EE" = here::here("inst", "qmd", "ee_report.qmd"),
  #   "EA" = here::here("inst", "qmd", "ea_report.qmd")
  # )

  path_list <- c(
    "MA" = system.file("qmd", "ma_report.qmd", package = "criticaldatafjp"),
    "AS" = system.file("qmd", "as_report.qmd", package = "criticaldatafjp"),
    "EA" = system.file("qmd", "ea_report.qmd", package = "criticaldatafjp"),
    "SE" = system.file("qmd", "se_report.qmd", package = "criticaldatafjp"),
    "HS" = system.file("qmd", "hs_report.qmd", package = "criticaldatafjp"),
    "SP" = system.file("qmd", "sp_report.qmd", package = "criticaldatafjp"),
    "DA" = system.file("qmd", "da_report.qmd", package = "criticaldatafjp"),
    "EO" = system.file("qmd", "eo_report.qmd", package = "criticaldatafjp"),
    "GP" = system.file("qmd", "gp_report.qmd", package = "criticaldatafjp"),
    "CA" = system.file("qmd", "ca_report.qmd", package = "criticaldatafjp"),
    "EL" = system.file("qmd", "el_report.qmd", package = "criticaldatafjp"),
    "FS" = system.file("qmd", "fs_report.qmd", package = "criticaldatafjp")
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
