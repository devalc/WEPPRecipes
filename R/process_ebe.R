#' Reads and processes event by event file
#'
#'This function reads the event by event file and returns
#'a dataframe that also includes sediment and phosphorus
#'variables in tonnes
#'
#'
#'
#' @param ebe_path A string pointing to the location of the event by event file
#' @param SimStartDate Start date of the simulation
#' @param SimEndDate End date of the simulation
#' @return A event by event dataframe along with all the
#' sediment and Phosphours variables in tonnes
#' @export
#'
#'
process_ebe <- function(ebe_path, SimStartDate, SimEndDate){

  ## read channel and watershed water and sediment data

  ebe <- utils::read.table(ebe_path, skip = 9, header = F)

  ### set names of the dataframes

  colnames(ebe) <- c("Day_ebe", "Month_ebe", "Year_ebe", "P_ebe", "Runoff_ebe", "peak_ebe", "Sediment_ebe", "SRP_ebe", "PP_ebe", "TP_ebe")

  ## calcs
  ebe <- ebe %>% dplyr::mutate(Date = seq(from = as.Date(SimStartDate), to = as.Date(SimEndDate), by = 1),
                        WY = EflowStats::get_waterYear(Date),
                        Sediment_tonnes_ebe = Sediment_ebe/1000,
                        SRP_tonnes_ebe = SRP_ebe/1000,
                        PP_tonnes_ebe = PP_ebe/1000,
                        TP_tonnes_ebe = TP_ebe/1000) %>% dplyr::select(Day_ebe, Month_ebe, Year_ebe, Date, WY, everything())



  return(ebe)

}
