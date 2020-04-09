#' Merges all the daily simulated variables into one df
#'
#'This function reads the totalwatsed, channel water balance
#'and event by event dataframes and returns
#'a merged dataframe of simulated variables in depth units
#'
#' @param totalwatsed_df Name of the totalwatsed dataframe
#' @param chanwb_df Name of the channel water balance dataframe
#' @param ebe_df Name of the event by event dataframe
#' @param sim_P_df Name of the dataframe containing Phosphorus data
#' @return A merged dataframe with all variables also in the depth units
#' @export
#'
#'
merge_daily_Sim_Vars <- function(totalwatsed_df, chanwb_df, ebe_df, sim_P_df){
  daily<- dplyr::left_join(totalwatsed_df, chanwb_df, by = c("Date", "WY")) %>%
    dplyr::left_join(ebe_df,  by = c("Date", "WY")) %>%
    dplyr::left_join(sim_P_df,  by = c("Date", "WY"))
    #, ObsData_df
    #left_join(ObsData_df,  by = c("Date", "WY"))
  return(daily)
}
