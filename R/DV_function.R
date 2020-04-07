#' Returns DV coefficient
#'
#' @param Qobs A vector containing observed values
#' @param Qsim A vector containing simulated values
#'
#' @return  DV
#' @export
#'

DV_function <- function (Qobs, Qsim) {
  # original data:
  Qobs_ori <-Qobs
  Qsim_ori <-Qsim
  # throw away missing values (both obs and sim must have paired values)
  Qsim <- Qsim_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
  Qobs <- Qobs_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
  if (length(Qobs) == 0 || length(Qsim) == 0)
    return(NA)
  DV <-((sum(Qobs)- sum(Qsim))/sum(Qobs))*100
  return(DV)
}
