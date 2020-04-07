#' Returns average of observed values
#'
#' @param obs A vector containing observed values
#' @param sim A vector containing simulated values
#' @export

avg_obs_function <- function(obs, sim){
  mean(Qobs[which(!is.na(Qobs) & !is.na(Qsim))])
}
