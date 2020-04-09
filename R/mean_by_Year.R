#' Creates a annual mean df from daily df
#'
#'This function takes in the merged daily dataframe
#'containing all the variables from channel,  totalwatsed and
#'event by event dataframes and returns annual mean df
#'
#' @param dailydf A merged daily dataframes containing all the simulated varibles
#' @return A dataframe containing annual mean summary
#'
#' @export
#'
mean_by_Year <- function(dailydf){
  Annual_mean_df<- daily_df %>%
    dplyr::select(-DOY_wshed, -WY, -Date, -Year_chan, -Day_chan,
           -Chan_ID_chan, -Elmt_ID_chan) %>%
    dplyr::group_by(Y_wshed) %>%
    dplyr::summarise_all(.funs = mean) %>% dplyr::ungroup()
  return(as.data.frame(Annual_mean_df))
}
