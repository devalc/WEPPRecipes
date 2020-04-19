#' Reads observed data into a df
#'
#'  Takes in a observed data in this specific ordered columns and format:
#'  column1. Date   --format = "%m/%d/%Y"
#'  column2. Streamflow --Units mm
#'  column3. Suspended sediment concentration --Units tonnes/day
#'  column4. Total Phosphorus --Units kg/day
#'  column5. Soluble Reactive Phosphorus --Units kg/day
#'  column6. Particulate Phosphorus --Units kg/day
#'  column7. WY
#'  column8. Snow Water Equivalent --Units mm
#'
#'
#' @param fpath path to the csv file containing the daily observed variables
#' @return a dataframe of pbserved data
#'
#' @export
#'

readObsDat <- function(fpath){
  ObsData <- read.csv(fpath,stringsAsFactors = F)
  colnames(ObsData) <- c("Date", "Q_mm_obs", "SSC_tonnes_day_obs",
                         "TP_kg_day_obs",  "SRP_kg_day_obs", "PP_kg_day_obs",
                         "WY")
  ObsData$Date <- as.Date(ObsData$Date, format = "%m/%d/%Y")
  ObsData <- ObsData %>% dplyr::select(Date, WY, everything())
  return(ObsData)
}


# readObsDat <- function(fpath){
#   ObsData <- read.csv(fpath,stringsAsFactors = F)
#   colnames(ObsData) <- c("Date", "Q_mm_obs", "SSC_kg_day_obs",
#                          "TP_kg_day_obs",  "SRP_kg_day_obs", "PP_kg_day_obs",
#                          "SSC_tonnes_day_obs","SWE_mm_obs")
#   ObsData$Date <- as.Date(ObsData$Date, format = "%m/%d/%Y")
#   ObsData <- ObsData %>% dplyr::mutate(WY = EflowStats::get_waterYear(ObsData$Date)) %>%
#     dplyr::select(Date, WY, everything())
#   return(ObsData)
# }
