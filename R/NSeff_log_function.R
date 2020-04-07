#' Returns Nash–Sutcliffe model efficiency coefficient of logarithmic data
#'
#' @param Qobs A vector containing observed values
#' @param Qsim A vector containing simulated values
#' @return  NS
#' @export

NSeff_log_function <- function (Qobs, Qsim) {
  # avoid zero and negative discharge values
  Qobs[which(Qobs<=1)] = 1
  Qsim[which(Qsim<=1)] = 1
  # convert to become log values
  Qobs = log(Qobs)
  Qsim = log(Qsim)
  # original data:
  Qobs_ori <-Qobs
  Qsim_ori <-Qsim
  # throw away missing values (both obs and sim must have values)
  Qsim <- Qsim_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
  Qobs <- Qobs_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
  if (length(Qobs) == 0 || length(Qsim) == 0)
    return(NA)
  NS <- 1 - (sum((Qobs - Qsim)^2)/sum((Qobs - mean(Qobs))^2))
  return(NS)
}
