#' Reads and processes Channel water balance file
#'
#'This function reads the channel water balance file and returns
#'a dataframe of simulated variables in depth units
#'
#'
#' @param chanwb_path A string pointing to the location of
#' the channel water balance file
#' @param Wshed_Area_m2 Area of the simulated watershed in m2
#' @return A Channel water balace dataframe including streamflow in dpeth units
#' @export
#'
#'

process_WEPP_chanwb_out <- function(chanwb_path, Wshed_Area_m2){

  ## read channel and watershed water and sediment data

  chanwb <- read.table(chanwb_path, skip = 11, header = F)

  ### set names of the dataframes

  colnames(chanwb) <- c("Year_chan" , "Day_chan", "Elmt_ID_chan","Chan_ID_chan", "Inflow_chan", "Outflow_chan",
                        "Storage_chan", "Baseflow_chan", "Loss_chan", "Balance_chan")



  chanwb <- chanwb %>% mutate(Q_outlet_mm = (Outflow_chan/ Wshed_Area_m2 *1000),
                              originDate = as.Date(paste0(Year_chan, "-01-01"),tz = "UTC") - days(1),
                              Date = as.Date(Day_chan, origin = originDate, tz = "UTC"),
                              WY = get_waterYear(Date)) %>% select(-originDate) %>%
    select(Year_chan, Day_chan, Date, WY, everything())



  return(chanwb)

}
