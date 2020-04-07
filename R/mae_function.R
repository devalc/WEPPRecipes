#' Returns MAE
#'
#' @param obs A vector containing observed values
#' @param pred A vector containing simulated values
#' @export
#'
mae_function <- function(obs, pred){
  mean(abs(obs-pred),na.rm=T)
}
