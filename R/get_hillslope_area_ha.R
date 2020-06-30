#' Read master pass file and returns a dataframe containing area(ha) of all the simulated
#' hillslopes.
#'
#'
#'
#' @param pass_path A string pointing to the location of
#' the master pass file from the WEPP simulation
#' @return A dataframe containing area (ha) of each simulated hillslope
#' @export
#'
#'


get_hillslope_area_ha <- function(pass_path) {
  num_hslopes <- as.numeric(stringr::str_extract(i, "[[:digit:]]+"))
  b <- read.table(pass_path, skip = 11, nrows = num_hslopes)
  b <- b %>% dplyr::select(-V2) %>% dplyr::mutate(V2 = 1:num_hslopes,
                                                  Hillslope = paste(V1, V2, sep =
                                                                      "")) %>%
    dplyr::mutate(Area_ha = V9 * 0.0001,
                  Hillslope = tolower(Hillslope)) %>%
    dplyr::select(Hillslope, Area_ha)
  return(data.frame(b))
}
