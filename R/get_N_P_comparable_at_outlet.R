#' This Function reads all the chemicalplot.csv files created by get_N_P_chemicalplot function,
#' and sums up values for all hillslopes for each date in df and calculates watershed area weighted
#' N and P values. i.e. kg of N and P per ha of watershed area.
#'
#'
#' @param wepp_outputs_Dir A string pointing to the location of
#' the WEPP simulation output folder containing chemplot.out files
#' @param total_hillslope_area_ha Numeric value specifying total area of hillslopes in watershed(ha)
#' @import dplyr
#' @import purrr
#' @return A csv file containing kg per ha of watershed area values of all the processed variables (N, P)
#' @export
#'
#'


get_N_P_comparable_at_outlet <- name <- function(wepp_outputs_Dir, total_hillslope_area_ha) {

  list_csvs <- list.files(path = paste(wepp_outputs_Dir,"chemplot_csvs_all_hillslopes", sep = "/"),
                          pattern='*.csv',full.names = TRUE)

  csvdf <- purrr::map_df(list_csvs, read.csv) %>% dplyr::select(-Area_ha, -Hillslope)

  csvdf_day_sum<- aggregate(. ~Date, csvdf, sum, na.rm=TRUE)

  csvdf_day_sum <- csvdf_day_sum %>%
    dplyr::mutate_at(.vars = vars(2:6),~(./total_hillslope_area_ha))%>%
    dplyr::rename_at(vars(2:6), ~paste0(.,"_wshed_ha"))

  readr::write_csv(csvdf_day_sum,path = paste0(wepp_outputs_Dir,"/","Simulated_N_P_kg_ha_of_wshed.csv" ))

  write.table(csvdf_day_sum, file = paste0(wepp_outputs_Dir,"/","Simulated_N_P_kg_ha_of_wshed.txt" ), sep = ",",
              row.names = FALSE, col.names = FALSE, quote=FALSE)
}

