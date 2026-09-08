#' parse_regras
#' @description
#' Support function used to map specific rules to correspondent variables.
#'
#' @param regras_por_var Vector or list of rules with corresponding variables. Eg:regras_por_var <- "all" or regras_por_var <- c("outlier" = "all", "min" = "pib,populacao,area") or regras_por_var <- list(outlier = c("pib", "populacao"), min = "all", max = c("idh", "renda_per_capita")  )
#' @param todas_vars Vector with all variables in data base
#' @importFrom stats setNames
#' @import stats
#' @returns List with mapped variables and rules.

parse_regras <- function(regras_por_var, todas_vars) {

  if (identical(regras_por_var, "all")) {
    regras_por_var <- setNames(rep(list("all"), length(fns_regras)), names(fns_regras))
  }

  if (!is.list(regras_por_var)) regras_por_var <- as.list(regras_por_var)

  res <- lapply(names(regras_por_var), function(regra) {
    vars <- regras_por_var[[regra]]

    if (is.character(vars) && length(vars) == 1 && vars == "all") {
      vars <- todas_vars
    } else if (is.character(vars)) {
      vars <- trimws(unlist(strsplit(vars, ",")))
    }

    invalidas <- setdiff(vars, todas_vars)
    if (length(invalidas) > 0) {
      stop("Variáveis inválidas para regra '", regra, "': ",
           paste(invalidas, collapse = ", "))
    }

    list(regra = regra, vars = vars)
  })

  names(res) <- names(regras_por_var)
  res
}
