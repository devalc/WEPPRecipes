#' Returns Bias
#'
#' @param obs A vector containing observed values
#' @param pred A vector containing simulated values
#' @export
#'
#'
bias_function<- function(obs, pred){
  mean(pred[which(!is.na(obs) & !is.na(pred))]) - mean(obs[which(!is.na(obs) & !is.na(pred))])
}
