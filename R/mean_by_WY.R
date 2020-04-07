#' Creates a WY mean df from daily df
#'
#'This function takes in the merged daily dataframe
#'containing all the variables from channel,  totalwatsed and
#'event by event dataframes and returns WY mean df
#'
#' @param dailydf A merged daily dataframes containing all the simulated varibles
#' @return A dataframe containing WY mean summary
#'
#' @export

mean_by_WY <- function(dailydf){
  WY_mean_df <- daily_df %>%
    select(-DOY_wshed, -Y_wshed, -Date, -Year_chan, -Day_chan,
           -Chan_ID_chan, -Elmt_ID_chan) %>%
    group_by(WY) %>%
    summarise_all(.funs = mean) %>% ungroup()
  return(as.data.frame(WY_mean_df))
}
