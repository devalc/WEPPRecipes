#' Estimates Phosphorus load, phosphorus in runoff,
#' baseflow and lateralflow
#'
#'This function reads in totalwatsed file and a vector containing
#' paramter values, estimates Phosophorus and return
#'a dataframe
#'
#'
#' @param P_outlet_df Dataframe created by the create_var_df_P_outlet function
#' @param Wshed_area_m2 Area of the watershed in m2
#' @param par Vector containg parameter values (mg/l) in the specific order-
#' P_conc_Runoff, P_conc_Lateral, P_conc_Baseflow, P_conc_Sediment
#' @return A dataframe containg estimated phosporus load,
#'  phosphorus in runoff, baseflow and lateral flow at the outlet
#'
#' @export
#'
#'


est_P_at_outlet <- function(P_outlet_df, Wshed_area_m2, par){


  P_outlet <- P_outlet_df %>% mutate(P_load_mg_outlet = Sediment_ebe * par[4],
                                     P_runoff_mg_outlet = (Runoff_as_PercOf_Q_outlet_mm * Wshed_area_m2 * par[1]) ,
                                     P_lateral_mg_outlet = (Lateral_as_PercOf_Q_outlet_mm * Wshed_area_m2 * par[2]) ,
                                     P_baseflow_mg_outlet = (Baseflow_as_PercOf_Q_outlet_mm * Wshed_area_m2 * par[3]),
                                     TP_out_kg_day_outlet = (P_load_mg_outlet + P_runoff_mg_outlet + P_lateral_mg_outlet +P_baseflow_mg_outlet)/1000000,
                                     PP_out_kg_day_outlet = P_load_mg_outlet/1000000,
                                     SRP_out_kg_day_outlet = TP_out_kg_day_outlet-PP_out_kg_day_outlet )

  return(P_outlet)

}
