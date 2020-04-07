#' Returns R2
#'
#' @param obs A vector containing observed values
#' @param pred A vector containing simulated values
#' @export
#'
R2_function <- function(obs, pred){
  summary(lm(obs ~ pred))$r.squared}
