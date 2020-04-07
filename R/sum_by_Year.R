#' Creates a annual sum df from daily df
#'
#'This function takes in the merged daily dataframe
#'containing all the variables from channel,  totalwatsed and
#'event by event dataframes and returns annual sum df
#'
#' @param dailydf A merged daily dataframes containing all the simulated varibles
#' @return A dataframe containing annual sum summary
#'
#' @export
#'
sum_by_Year <- function(dailydf){
  Annual_sum_df<- daily_df %>%
    select(-DOY_wshed, -WY, -Date, -Year_chan, -Day_chan,
           -Chan_ID_chan, -Elmt_ID_chan) %>%
    group_by(Y_wshed) %>%
    summarise_all(.funs = sum) %>% ungroup()
  return(as.data.frame(Annual_sum_df))
}
