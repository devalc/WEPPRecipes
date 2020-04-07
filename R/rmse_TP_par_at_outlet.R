#' Returns RMSE between estimated and observed total phosphorus
#' at the watershed outlet
#'
#' @param P_outlet_df Dataframe created by the create_var_df_P_outlet function
#' @param Wshed_area_m2 Area of the watershed in m2
#' @param par Vector containg parameter values (mg/l) in the specific order-
#' P_conc_Runoff, P_conc_Lateral, P_conc_Baseflow, P_conc_Sediment
#' @return rmse between observed and simulated Total phosphorus at the watershed outlet
#'
#' @export
#'
#'



rmse_TP_par_at_outlet <- function(P_outlet_df, Wshed_area_m2, par){

  P_outlet_df <- as.data.frame(P_outlet_df)

  P_outlet <- P_outlet_df %>% mutate(P_load_mg_outlet = Sediment_ebe * par[4],
                                     P_runoff_mg_outlet = (Runoff_as_PercOf_Q_outlet_mm * Wshed_area_m2 * par[1]) ,
                                     P_lateral_mg_outlet = (Lateral_as_PercOf_Q_outlet_mm * Wshed_area_m2 * par[2]) ,
                                     P_baseflow_mg_outlet = (Baseflow_as_PercOf_Q_outlet_mm * Wshed_area_m2 * par[3]),
                                     TP_out_kg_day_outlet = (P_load_mg_outlet + P_runoff_mg_outlet + P_lateral_mg_outlet +P_baseflow_mg_outlet)/1000000,
                                     PP_out_kg_day_outlet = P_load_mg_outlet/1000000,
                                     SRP_out_kg_day_outlet = TP_out_kg_day_outlet-PP_out_kg_day_outlet )


  return(rmse_function(as.numeric(P_outlet$TP_kg_day_obs), as.numeric(P_outlet$TP_out_kg_day)))

}
