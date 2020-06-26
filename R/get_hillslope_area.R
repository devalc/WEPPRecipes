#' Read master pass file and returns a dataframe containing area(ha) of all the simulated
#' hillslopes.
#'
#'
#'
#' @param pass_file_path A string pointing to the location of
#' the master pass file from the WEPP simulation
#' @param Number_of_Hillslopes_simulated Numeric value specifying the number of hillslopes simulated
#' @return A dataframe containing area (ha) of each simulated hillslope
#' @export
#'
#'

get_hillslope_area <- function(pass_file_path, Number_of_Hillslopes_simulated){
  b<- read.table(pass_file_path, skip = 11, nrows = Number_of_Hillslopes_simulated)
  b <- b %>% dplyr::mutate(Hillslope = paste(V1, V2, sep="")) %>%
    dplyr::mutate(Area_ha = V9*0.0001,
           Hillslope = tolower(Hillslope)) %>%
    dplyr::select(Hillslope, Area_ha)
  return(data.frame(b))
}
