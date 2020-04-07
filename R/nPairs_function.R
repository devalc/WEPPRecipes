#' Returns number of complete pairs
#'
#' @param obs A vector containing observed values
#' @param pred A vector containing simulated values
#' @export



nPairs_function <- function(obs, pred){
  length(pred[which(!is.na(obs) & !is.na(pred))])
}
