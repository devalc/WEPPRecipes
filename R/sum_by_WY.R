#' Creates a WY sum df from daily df
#'
#'This function takes in the merged daily dataframe
#'containing all the variables from channel,  totalwatsed and
#'event by event dataframes and returns WY sum df
#'
#' @param dailydf A merged daily dataframes containing all the simulated varibles
#' @return A dataframe containing WY sum summary
#'
#' @export

sum_by_WY <- function(dailydf){
  WY_sum_df <- daily_df %>%
    select(-DOY_wshed, -Y_wshed, -Date, -Year_chan, -Day_chan,
           -Chan_ID_chan, -Elmt_ID_chan) %>%
    group_by(WY) %>%
    summarise_all(.funs = sum) %>% ungroup()
  return(as.data.frame(WY_sum_df))
}
