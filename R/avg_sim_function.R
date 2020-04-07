#' Returns average of simulated values
#'
#' @param obs A vector containing observed values
#' @param sim A vector containing simulated values
#' @export
#'
#'
#'
avg_sim_function <- function(obs, sim){
  mean(Qsim[which(!is.na(Qobs) & !is.na(Qsim))])
}
