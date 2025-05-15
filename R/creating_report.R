#' @title Create Report
#' @description
#' This function return the report with all statistical analysis and critial rules
#'
#' @param path_data Path to dataset
#' @param mydir Output directory
#' @param knitmydir output knit
#' @return Report
#' @export

creating_report <- function(mydir=getwd(), knitmydir = getwd()){

  path_list <- c(
    "MA" = system.file("rmd", "report_ma.Rmd", package = "dataeditfjp"),
    "AS" = system.file("rmd", "report_as.Rmd", package = "dataeditfjp"),
    "EA" = system.file("rmd", "report_ea.Rmd", package = "dataeditfjp"),
    "SE" = system.file("rmd", "report_se.Rmd", package = "dataeditfjp"),
    "HS" = system.file("rmd", "report_hs.Rmd", package = "dataeditfjp"),
    "SP" = system.file("rmd", "report_sp.Rmd", package = "dataeditfjp"),
    "DA" = system.file("rmd", "report_da.Rmd", package = "dataeditfjp"),
    "EO" = system.file("rmd", "report_eo.Rmd", package = "dataeditfjp"),
    "GP" = system.file("rmd", "report_gp.Rmd", package = "dataeditfjp"),
    "CA" = system.file("rmd", "report_ca.Rmd", package = "dataeditfjp"),
    "EL" = system.file("rmd", "report_el.Rmd", package = "dataeditfjp"),
    "FS" = system.file("rmd", "report_fs.Rmd", package = "dataeditfjp"),
    "SN" = system.file("rmd", "report_sn.Rmd", package = "dataeditfjp"),
    "EC" = system.file("rmd", "report_ec.Rmd", package = "dataeditfjp")
  )


  # Prompt para o usuário selecionar a base de dados
  path_data <- file.choose()

  # Obter o prefixo das variáveis (identificar a dimensão)
  prefix <- substr(rev(colnames(openxlsx::read.xlsx(path_data, rows = 1)))[1], 1, 2)

  rmarkdown::render(input = path_list[prefix],knit_root_dir = knitmydir, output_dir = mydir, params = list(data = path_data))

}

