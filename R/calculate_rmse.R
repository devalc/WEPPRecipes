#' Calculate Root Mean Squared Error (RMSE) between simulated and observed data
#'
#'
#' @param obs vector containing observed values
#' @param pred vector containing Simulated values
#' @return RMSE between observed and simulated values
#'
#' @export
#'
#'

rmse_function <- function(obs, pred){
                          sqrt(mean((obs-pred)^2 ,na.rm=T))}
