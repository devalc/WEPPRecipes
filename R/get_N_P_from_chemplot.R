#' This Function reads all the chemplot.out files from the WEPP simulation output folder,
#' writes hillslope number and date to each hillslope chemplot file, extracts and calculate Nitrogen
#' and Phosphorus variables in kg units and exports to individual csv file
#'
#'
#' @param output_folder A string pointing to the location of
#' the WEPP simulation output folder containing chemplot.out files
#' @param hillslope_area_df A data frame containing hillslope areas (retrived using get_hillslope_area function)
#' @param SimStartDate A string specifying simulation start date as "YYYY-MM-DD" (eg: "1990-01-01")
#' @param SimEndDate A string specifying simulation end date as "YYYY-MM-DD" (eg: "2010-12-31")
#' @import dplyr
#' @return A csv file containing all the processed values for each hillslope written
#' in the output_folder
#' @export
#'
#'

get_N_P_from_chemplot <- function(output_folder,hillslope_area_df,
                                      SimStartDate, SimEndDate){
  file.list <- list.files(path = output_folder, pattern='chemplot_*.txt',full.names = F)
  for (i in file.list) {
    hillslpNo <- as.numeric(stringr::str_extract(i, "[[:digit:]]+"))
    # print(hillslpNo)
    df <- read.table(paste(output_folder, i, sep = "/"), skip = 2) %>%
      dplyr::mutate(Hillslope = paste("hillslope", hillslpNo, sep = ""),
                    Date = seq(from = as.Date(SimStartDate), to = as.Date(SimEndDate), by = 1)) %>%
      dplyr::select(Date, Hillslope, V8, V9, V10, V20, V21) %>%
      dplyr::rename("Date" = "Date",
                    "Hillslope" ="Hillslope",
                    "NLeached"=V8,
                    "NSediments"= V9,
                    "NRunoff"=V10,
                    "PSediments"=V20,
                    "PRunoff"=V21)

    df$NLeached <- as.numeric(df$NLeached)
    df$NSediments <- as.numeric(df$NSediments)
    df$NRunoff <- as.numeric(df$NRunoff)
    df$PSediments <- as.numeric(df$PSediments)
    df$PRunoff <- as.numeric(df$PRunoff)

    df <- dplyr::left_join(df, hillslope_area_df, by= c("Hillslope"))

    df <- df %>%
      dplyr::mutate_at(.vars = dplyr::vars(NLeached,NSediments,NRunoff,PSediments,PRunoff),~(.*Area_ha))%>%
      dplyr::rename_at(dplyr::vars(NLeached,NSediments,NRunoff,PSediments,PRunoff), ~paste0(.,"_kg"))

    readr::write_csv(df,path = paste0(output_folder,"/", i , ".csv" ))
  }}
