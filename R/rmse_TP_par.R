#' Returns RMSE between  estimated and observed total phosphorus
#'
#'
#' @param totalwatsed_df dataframe containing totalwatsed file
#' @param Observed_P_kg_day dataframe containing
#' @param par Vector containg parameter values (mg/l) in the specific order-
#' P_conc_Runoff, P_conc_Lateral, P_conc_Baseflow, P_conc_Sediment
#' @return rmse between observed and simulated Total phosphorus
#'
#' @export
#'
#'
rmse_TP_par <- function(totalwatsed_df, Observed_P_kg_day, par){
  ###par order P_conc_Runoff_guess_mg_l, P_conc_Lateral_guess_mg_l, P_conc_Baseflow_guess_mg_l, P_conc_Sediment_guess_mg_l
  P_calc <- totalwatsed_df %>% dplyr::select(Date, WY, Area_m2_wshed, Sed_Del_kg_wshed,
                                      Runoff_m3_wshed_to_mm, Lateral_m3_wshed_to_mm, Baseflow_mm_wshed )

  P_calc <- dplyr::left_join(P_calc, Observed_P_kg_day, by=c("Date", "WY"))

  P_calc <- P_calc %>%
    dplyr::mutate(P_load_mg = Sed_Del_kg_wshed * par[4],
           P_runoff_mg = (Runoff_m3_wshed_to_mm * Area_m2_wshed * par[1]) ,
           P_lateral_mg = (Lateral_m3_wshed_to_mm * Area_m2_wshed * par[2]) ,
           P_baseflow_mg = (Baseflow_mm_wshed * Area_m2_wshed * par[3]),
           TP_hill_kg_day = (P_load_mg + P_runoff_mg + P_lateral_mg +P_baseflow_mg)/1000000,
           PP_hill_kg_day = P_load_mg/1000000,
           SRP_hill_kg_day = TP_hill_kg_day - PP_hill_kg_day)



  return(rmse_function(as.numeric(P_calc$TP_kg_day_obs), as.numeric(P_calc$TP_hill_kg_day)))

}
