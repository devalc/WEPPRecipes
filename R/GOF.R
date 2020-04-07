#' Returns a goodness of fit statistics for Streamflow, SWE, Phosphorus
#' and sediment.
#'
#'  Takes in a dataframe containing both observed and simulated variables
#'  (of identical length) and returns NSE, DV, KGE, MAE, RMSE, R2 and R2adj
#'  for each variable namely, Streamflow, SWE, Phosphorus and sediment.
#'
#' @param df dataframe containing observed and sumulated data for  Streamflow, SWE, Phosphorus and sediment.
#' @return Goodness of Fit between observed and simulated data
#'
#' @export
#'
#'
GOF <- function(df){
  alltime_GOF_SF <- df %>% dplyr::summarise(nPairs = nPairs_function(Q_mm_obs, Q_outlet_mm),
                                     NSE = NSeff_function(Q_mm_obs, Q_outlet_mm),
                                     DV = DV_function(Q_mm_obs, Q_outlet_mm),
                                     KGE = hydroGOF::KGE(Q_mm_obs, Q_outlet_mm),
                                     MAE = hydroGOF::mae(Q_mm_obs, Q_outlet_mm),
                                     RMSE = rmse_function(Q_mm_obs, Q_outlet_mm),
                                     R2 = R2_function(Q_mm_obs, Q_outlet_mm),
                                     R2adj = R2ad_function(Q_mm_obs, Q_outlet_mm)
  )
  alltime_GOF_SWE <- df %>% dplyr::summarise(nPairs = nPairs_function(SWE_mm_obs, SimSWE_mm_wshed),
                                      NSE = NSeff_function(SWE_mm_obs, SimSWE_mm_wshed),
                                      DV = DV_function(SWE_mm_obs, SimSWE_mm_wshed),
                                      KGE = hydroGOF::KGE(SWE_mm_obs, SimSWE_mm_wshed),
                                      MAE = hydroGOF::mae(SWE_mm_obs, SimSWE_mm_wshed),
                                      RMSE = rmse_function(SWE_mm_obs, SimSWE_mm_wshed),
                                      R2 = R2_function(SWE_mm_obs, SimSWE_mm_wshed),
                                      R2adj = R2ad_function(SWE_mm_obs, SimSWE_mm_wshed)
  )
  alltime_GOF_TP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(TP_kg_day_obs, TP_hill_kg_day),
                                               NSE = NSeff_function(TP_kg_day_obs, TP_hill_kg_day),
                                               DV = DV_function(TP_kg_day_obs, TP_hill_kg_day),
                                               KGE = hydroGOF::KGE(TP_kg_day_obs, TP_hill_kg_day),
                                               MAE = hydroGOF::mae(TP_kg_day_obs, TP_hill_kg_day),
                                               RMSE = rmse_function(TP_kg_day_obs, TP_hill_kg_day),
                                               R2 = R2_function(TP_kg_day_obs, TP_hill_kg_day),
                                               R2adj = R2ad_function(TP_kg_day_obs, TP_hill_kg_day)
  )
  alltime_GOF_PP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(PP_kg_day_obs, PP_hill_kg_day),
                                               NSE = NSeff_function(PP_kg_day_obs, PP_hill_kg_day),
                                               DV = DV_function(PP_kg_day_obs, PP_hill_kg_day),
                                               KGE = hydroGOF::KGE(PP_kg_day_obs, PP_hill_kg_day),
                                               MAE = hydroGOF::mae(PP_kg_day_obs, PP_hill_kg_day),
                                               RMSE = rmse_function(PP_kg_day_obs, PP_hill_kg_day),
                                               R2 = R2_function(PP_kg_day_obs, PP_hill_kg_day),
                                               R2adj = R2ad_function(PP_kg_day_obs, PP_hill_kg_day)
  )
  alltime_GOF_SRP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(SRP_kg_day_obs, SRP_hill_kg_day),
                                                NSE = NSeff_function(SRP_kg_day_obs, SRP_hill_kg_day),
                                                DV = DV_function(SRP_kg_day_obs, SRP_hill_kg_day),
                                                KGE = hydroGOF::KGE(SRP_kg_day_obs, SRP_hill_kg_day),
                                                MAE = hydroGOF::mae(SRP_kg_day_obs, SRP_hill_kg_day),
                                                RMSE = rmse_function(SRP_kg_day_obs, SRP_hill_kg_day),
                                                R2 = R2_function(SRP_kg_day_obs, SRP_hill_kg_day),
                                                R2adj = R2ad_function(SRP_kg_day_obs, SRP_hill_kg_day)
  )
  alltime_GOF_Sediment_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed),
                                                     NSE = NSeff_function(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed),
                                                     DV = DV_function(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed),
                                                     KGE = hydroGOF::KGE(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed),
                                                     MAE = hydroGOF::mae(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed),
                                                     RMSE = rmse_function(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed),
                                                     R2 = R2_function(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed),
                                                     R2adj = R2ad_function(SSC_tonnes_day_obs, SimSediment_tonnes_day_wshed)
  )
  alltime_GOF_TP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(TP_kg_day_obs, TP_out_kg_day_outlet),
                                          NSE = NSeff_function(TP_kg_day_obs, TP_out_kg_day_outlet),
                                          DV = DV_function(TP_kg_day_obs, TP_out_kg_day_outlet),
                                          KGE = hydroGOF::KGE(TP_kg_day_obs, TP_out_kg_day_outlet),
                                          MAE = hydroGOF::mae(TP_kg_day_obs, TP_out_kg_day_outlet),
                                          RMSE = rmse_function(TP_kg_day_obs, TP_out_kg_day_outlet),
                                          R2 = R2_function(TP_kg_day_obs, TP_out_kg_day_outlet),
                                          R2adj = R2ad_function(TP_kg_day_obs, TP_out_kg_day_outlet)
  )
  alltime_GOF_PP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(PP_kg_day_obs, PP_out_kg_day_outlet),
                                          NSE = NSeff_function(PP_kg_day_obs, PP_out_kg_day_outlet),
                                          DV = DV_function(PP_kg_day_obs, PP_out_kg_day_outlet),
                                          KGE = hydroGOF::KGE(PP_kg_day_obs, PP_out_kg_day_outlet),
                                          MAE = hydroGOF::mae(PP_kg_day_obs, PP_out_kg_day_outlet),
                                          RMSE = rmse_function(PP_kg_day_obs, PP_out_kg_day_outlet),
                                          R2 = R2_function(PP_kg_day_obs, PP_out_kg_day_outlet),
                                          R2adj = R2ad_function(PP_kg_day_obs, PP_out_kg_day_outlet)
  )
  alltime_GOF_SRP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(SRP_kg_day_obs, SRP_out_kg_day_outlet),
                                           NSE = NSeff_function(SRP_kg_day_obs, SRP_out_kg_day_outlet),
                                           DV = DV_function(SRP_kg_day_obs, SRP_out_kg_day_outlet),
                                           KGE = hydroGOF::KGE(SRP_kg_day_obs, SRP_out_kg_day_outlet),
                                           MAE = hydroGOF::mae(SRP_kg_day_obs, SRP_out_kg_day_outlet),
                                           RMSE = rmse_function(SRP_kg_day_obs, SRP_out_kg_day_outlet),
                                           R2 = R2_function(SRP_kg_day_obs, SRP_out_kg_day_outlet),
                                           R2adj = R2ad_function(SRP_kg_day_obs, SRP_out_kg_day_outlet)
  )
  alltime_GOF_Sediment_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(SSC_tonnes_day_obs, Sediment_tonnes_ebe),
                                                NSE = NSeff_function(SSC_tonnes_day_obs, Sediment_tonnes_ebe),
                                                DV = DV_function(SSC_tonnes_day_obs, Sediment_tonnes_ebe),
                                                KGE = hydroGOF::KGE(SSC_tonnes_day_obs, Sediment_tonnes_ebe),
                                                MAE = hydroGOF::mae(SSC_tonnes_day_obs, Sediment_tonnes_ebe),
                                                RMSE = rmse_function(SSC_tonnes_day_obs, Sediment_tonnes_ebe),
                                                R2 = R2_function(SSC_tonnes_day_obs, Sediment_tonnes_ebe),
                                                R2adj = R2ad_function(SSC_tonnes_day_obs, Sediment_tonnes_ebe)
  )

  a <- data.frame(alltime_GOF_SF, row.names = "Streamflow(mm)")
  b <- data.frame(alltime_GOF_SWE, row.names = "SWE(mm)")
  c <- data.frame(alltime_GOF_TP_hillslope, row.names = "TP_hillslope(kg/day)")
  d <- data.frame(alltime_GOF_PP_hillslope, row.names = "PP_hillslope(kg/day)")
  e <- data.frame(alltime_GOF_SRP_hillslope, row.names = "SRP_hillslope(kg/day)")
  f<- data.frame(alltime_GOF_Sediment_hillslope, row.names = "Sediment_hillslope(tonnes/day)")
  g <- data.frame(alltime_GOF_TP_chan, row.names = "TP_channel(kg/day)")
  h <- data.frame(alltime_GOF_PP_chan, row.names = "PP_channel(kg/day)")
  i <- data.frame(alltime_GOF_SRP_chan, row.names = "SRP_channel(kg/day)")
  j <- data.frame(alltime_GOF_Sediment_chan, row.names = "Sediment_channel(tonnes/day)")
  return(rbind(a,b,c,d,e,f,g,h,i,j))
}
