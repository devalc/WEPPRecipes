#' Creates a dataframe containing subset of variables required
#' to calculate Phosphorus at the outlet
#'
#'This function reads merged all variables file and a event
#'by event file along with the observed phosphorus data
#'of same length/time period and returns a dataframe
#'
#' @param all_var_df Name of the dataframe containing all the simulated variables
#' @param ebe_var_df Name of the event by event dataframe
#' @param Obs_P Name of the dataframe conining observed P data
#' @return A merged with all variables required for P calculations at the outlet
#' @export
#'
#'


create_var_df_P_outlet <- function(all_var_df, ebe_var_df, Obs_P){

  ebe_sed <- ebe_var_df %>% dplyr::select(Date, WY, Sediment_ebe)

  P_outlet <- all_var_df %>% dplyr::select(Date, WY, Q_outlet_mm,Runoff_as_PercOf_Q_outlet_mm,
                                  Lateral_as_PercOf_Q_outlet_mm, Baseflow_as_PercOf_Q_outlet_mm )

  P_outlet <- dplyr::left_join(P_outlet, ebe_sed, by = c("Date", "WY"))
  P_outlet <- dplyr::left_join(P_outlet, Obs_P, by = c("Date", "WY"))

  return(P_outlet)
}
