#' @title get output files
#'
#' @param output_path Path representing the working directory
#'
#' @return

get_files <- function(output_path){
  path_file <- system.file("qmd", package = "dataeditfjp")

  pattern_xlsx = "_violacoes.xlsx$"
  pattern_html = "_relatorio.html$"
  id_xlsx <- grepl(pattern_xlsx,list.files(path_file))
  id_html <- grepl(pattern_html,list.files(path_file))

  output_xlsx <- list.files(path_file,full.names = T)[id_xlsx]
  output_html <- list.files(path_file,full.names = T)[id_html]

  file.copy(output_xlsx,output_path)
  file.copy(output_html,output_path)
  if(file.exists(output_xlsx)){file.remove(output_xlsx)}
  if(file.exists(output_html)){file.remove(output_html)}

}
