#' Returns RMSE
#'
#' @param obs A vector containing observed values
#' @param pred A vector containing simulated values
#' @export
#'
#'

rmse_function <- function(obs, pred){
  sqrt(mean((obs-pred)^2 ,na.rm=T))}
