#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#'
#' @return Report
#' @export

## To-do: criar uma função para mover o relatório "relatorio.html" do diretório
## \inst\qmd para a raíz do projeto, dado que quarto::quarto_render() não oferece
## esta opção
create_report <- function(){
  # Mapeamento dos relatórios
  path_list <- c(
    "EA" = here::here("inst", "qmd", "ea_report.qmd"),
    "EE" = here::here("inst", "qmd", "ee_report.qmd"),
    "MA" = here::here("inst", "qmd", "ma_report.qmd"),
    "SP" = here::here("inst", "qmd", "sp_report.qmd")
  )
  # path_list <- c(
  #   "MA" = system.file("inst", "qmd", "ma_report.qmd", package = "criticaldatafjp"),
  #   "EE" = system.file("inst", "qmd", "ee_report.qmd", package = "criticaldatafjp"),
  #   "EA" = system.file("inst", "qmd", "ea_report.qmd", package = "criticaldatafjp")
  # )

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
