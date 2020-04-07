#' Estimates Phosphorus load, phosphorus in runoff,
#' baseflow and lateralflow
#'
#'This function reads in totalwatsed file and a vector containing
#' paramter values, estimates Phosophorus and return
#'a dataframe
#'
#'
#' @param totalwatsed_df dataframe containing totalwatsed file
#' @param par Vector containg parameter values (mg/l) in the specific order -
#' P_conc_Runoff, P_conc_Lateral, P_conc_Baseflow, P_conc_Sediment
#' @return A dataframe containg estimated phosporus load,
#'  phosphorus in runoff, baseflow and lateralflow
#'
#' @export
#'
#'
est_P <- function(totalwatsed_df, par){
  ###par order P_conc_Runoff_guess_mg_l, P_conc_Lateral_guess_mg_l, P_conc_Baseflow_guess_mg_l, P_conc_Sediment_guess_mg_l
  P_calc <- totalwatsed_df %>% select(Date, WY, Area_m2_wshed, Sed_Del_kg_wshed,
                                      Runoff_m3_wshed_to_mm, Lateral_m3_wshed_to_mm, Baseflow_mm_wshed ) %>%
    mutate(P_load_mg = Sed_Del_kg_wshed * par[4],
           P_runoff_mg = (Runoff_m3_wshed_to_mm * Area_m2_wshed * par[1]) ,
           P_lateral_mg = (Lateral_m3_wshed_to_mm * Area_m2_wshed * par[2]) ,
           P_baseflow_mg = (Baseflow_mm_wshed * Area_m2_wshed * par[3]),
           TP_hill_kg_day = (P_load_mg + P_runoff_mg + P_lateral_mg +P_baseflow_mg)/1000000,
           PP_hill_kg_day = P_load_mg/1000000,
           SRP_hill_kg_day = TP_hill_kg_day - PP_hill_kg_day) %>% select(-Area_m2_wshed, -Sed_Del_kg_wshed, -Runoff_m3_wshed_to_mm,
                                                                         -Lateral_m3_wshed_to_mm, -Baseflow_mm_wshed)

  return(P_calc)

}
