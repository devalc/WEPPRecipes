#' Extracts watershed area from the loss file
#'
#'This function reads the loss file, extracts and
#'returns area of the simulated watersehd in square meters
#'
#'
#' @param file A string point to the path of the loss file
#' @return A numeric vector of length 1 containing the watershed area
#' @export
#'
#'
get_WatershedArea_m2 <- function(file){
  getstring<- grep("Total contributing area to outlet ",
                   readLines(file), value = TRUE)
  getstring <- getstring[[1]]
  num <- readr::parse_number(getstring)
  num <- as.numeric(num) * 10000 ##convert ha to m2
  return(num)

}
