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
  alltime_GOF_SF <- df %>% dplyr::summarise(nPairs = nPairs_function(df$Q_mm_obs, df$Q_outlet_mm),
                                            NSE = NSeff_function(df$Q_mm_obs, df$Q_outlet_mm),
                                            DV = DV_function(df$Q_mm_obs, df$Q_outlet_mm),
                                            KGE = hydroGOF::KGE(as.numeric(df$Q_mm_obs), as.numeric(df$Q_outlet_mm)),
                                            MAE = hydroGOF::mae(df$Q_mm_obs, df$Q_outlet_mm),
                                            RMSE = rmse_function(df$Q_mm_obs, df$Q_outlet_mm),
                                            R2 = R2_function(df$Q_mm_obs, df$Q_outlet_mm),
                                            R2adj = R2ad_function(df$Q_mm_obs, df$Q_outlet_mm)
  )
  alltime_GOF_TP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
                                                      NSE = NSeff_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
                                                      DV = DV_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
                                                      KGE = hydroGOF::KGE(as.numeric(df$TP_kg_day_obs), as.numeric(df$TP_hill_kg_day)),
                                                      MAE = hydroGOF::mae(df$TP_kg_day_obs, df$TP_hill_kg_day),
                                                      RMSE = rmse_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
                                                      R2 = R2_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
                                                      R2adj = R2ad_function(df$TP_kg_day_obs, df$TP_hill_kg_day)
  )
  alltime_GOF_PP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
                                                      NSE = NSeff_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
                                                      DV = DV_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
                                                      KGE = hydroGOF::KGE(as.numeric(df$PP_kg_day_obs), as.numeric(df$PP_hill_kg_day)),
                                                      MAE = hydroGOF::mae(df$PP_kg_day_obs, df$PP_hill_kg_day),
                                                      RMSE = rmse_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
                                                      R2 = R2_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
                                                      R2adj = R2ad_function(df$PP_kg_day_obs, df$PP_hill_kg_day)
  )
  alltime_GOF_SRP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
                                                       NSE = NSeff_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
                                                       DV = DV_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
                                                       KGE = hydroGOF::KGE(as.numeric(df$SRP_kg_day_obs), as.numeric(df$SRP_hill_kg_day)),
                                                       MAE = hydroGOF::mae(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
                                                       RMSE = rmse_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
                                                       R2 = R2_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
                                                       R2adj = R2ad_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day)
  )
  alltime_GOF_Sediment_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
                                                            NSE = NSeff_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
                                                            DV = DV_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
                                                            KGE = hydroGOF::KGE(as.numeric(as.numeric(df$SSC_tonnes_day_obs)), as.numeric(df$SimSediment_tonnes_day_wshed)),
                                                            MAE = hydroGOF::mae(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
                                                            RMSE = rmse_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
                                                            R2 = R2_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
                                                            R2adj = R2ad_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed)
  )
  alltime_GOF_TP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
                                                 NSE = NSeff_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
                                                 DV = DV_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
                                                 KGE = hydroGOF::KGE(as.numeric(df$TP_kg_day_obs), as.numeric(df$TP_out_kg_day_outlet)),
                                                 MAE = hydroGOF::mae(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
                                                 RMSE = rmse_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
                                                 R2 = R2_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
                                                 R2adj = R2ad_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet)
  )
  alltime_GOF_PP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
                                                 NSE = NSeff_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
                                                 DV = DV_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
                                                 KGE = hydroGOF::KGE(as.numeric(df$PP_kg_day_obs), as.numeric(df$PP_out_kg_day_outlet)),
                                                 MAE = hydroGOF::mae(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
                                                 RMSE = rmse_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
                                                 R2 = R2_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
                                                 R2adj = R2ad_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet)
  )
  alltime_GOF_SRP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
                                                  NSE = NSeff_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
                                                  DV = DV_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
                                                  KGE = hydroGOF::KGE(as.numeric(df$SRP_kg_day_obs), as.numeric(df$SRP_out_kg_day_outlet)),
                                                  MAE = hydroGOF::mae(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
                                                  RMSE = rmse_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
                                                  R2 = R2_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
                                                  R2adj = R2ad_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet)
  )
  alltime_GOF_Sediment_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
                                                       NSE = NSeff_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
                                                       DV = DV_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
                                                       KGE = hydroGOF::KGE(as.numeric(df$SSC_tonnes_day_obs), as.numeric(df$Sediment_tonnes_ebe)),
                                                       MAE = hydroGOF::mae(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
                                                       RMSE = rmse_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
                                                       R2 = R2_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
                                                       R2adj = R2ad_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe)
  )

  a <- data.frame(alltime_GOF_SF, row.names = "Streamflow(mm)")
  # b <- data.frame(alltime_GOF_SWE, row.names = "SWE(mm)")
  c <- data.frame(alltime_GOF_TP_hillslope, row.names = "TP_hillslope(kg/day)")
  d <- data.frame(alltime_GOF_PP_hillslope, row.names = "PP_hillslope(kg/day)")
  e <- data.frame(alltime_GOF_SRP_hillslope, row.names = "SRP_hillslope(kg/day)")
  f<- data.frame(alltime_GOF_Sediment_hillslope, row.names = "Sediment_hillslope(tonnes/day)")
  g <- data.frame(alltime_GOF_TP_chan, row.names = "TP_channel(kg/day)")
  h <- data.frame(alltime_GOF_PP_chan, row.names = "PP_channel(kg/day)")
  i <- data.frame(alltime_GOF_SRP_chan, row.names = "SRP_channel(kg/day)")
  j <- data.frame(alltime_GOF_Sediment_chan, row.names = "Sediment_channel(tonnes/day)")
  return(rbind(a,c,d,e,f,g,h,i,j))
}


#
# GOF <- function(df){
#   alltime_GOF_SF <- df %>% dplyr::summarise(nPairs = nPairs_function(df$Q_mm_obs, df$Q_outlet_mm),
#                                      NSE = NSeff_function(df$Q_mm_obs, df$Q_outlet_mm),
#                                      DV = DV_function(df$Q_mm_obs, df$Q_outlet_mm),
#                                      KGE = hydroGOF::KGE(as.numeric(df$Q_mm_obs), as.numeric(df$Q_outlet_mm)),
#                                      MAE = hydroGOF::mae(df$Q_mm_obs, df$Q_outlet_mm),
#                                      RMSE = rmse_function(df$Q_mm_obs, df$Q_outlet_mm),
#                                      R2 = R2_function(df$Q_mm_obs, df$Q_outlet_mm),
#                                      R2adj = R2ad_function(df$Q_mm_obs, df$Q_outlet_mm)
#   )
#   alltime_GOF_SWE <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SWE_mm_obs, df$SimSWE_mm_wshed),
#                                       NSE = NSeff_function(df$SWE_mm_obs, df$SimSWE_mm_wshed),
#                                       DV = DV_function(df$SWE_mm_obs, df$SimSWE_mm_wshed),
#                                       KGE = hydroGOF::KGE(as.numeric(df$SWE_mm_obs), as.numeric(df$SimSWE_mm_wshed)),
#                                       MAE = hydroGOF::mae(df$SWE_mm_obs, df$SimSWE_mm_wshed),
#                                       RMSE = rmse_function(df$SWE_mm_obs, df$SimSWE_mm_wshed),
#                                       R2 = R2_function(df$SWE_mm_obs, df$SimSWE_mm_wshed),
#                                       R2adj = R2ad_function(df$SWE_mm_obs, df$SimSWE_mm_wshed)
#   )
#   alltime_GOF_TP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
#                                                NSE = NSeff_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
#                                                DV = DV_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
#                                                KGE = hydroGOF::KGE(as.numeric(df$TP_kg_day_obs), as.numeric(df$TP_hill_kg_day)),
#                                                MAE = hydroGOF::mae(df$TP_kg_day_obs, df$TP_hill_kg_day),
#                                                RMSE = rmse_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
#                                                R2 = R2_function(df$TP_kg_day_obs, df$TP_hill_kg_day),
#                                                R2adj = R2ad_function(df$TP_kg_day_obs, df$TP_hill_kg_day)
#   )
#   alltime_GOF_PP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
#                                                NSE = NSeff_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
#                                                DV = DV_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
#                                                KGE = hydroGOF::KGE(as.numeric(df$PP_kg_day_obs), as.numeric(df$PP_hill_kg_day)),
#                                                MAE = hydroGOF::mae(df$PP_kg_day_obs, df$PP_hill_kg_day),
#                                                RMSE = rmse_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
#                                                R2 = R2_function(df$PP_kg_day_obs, df$PP_hill_kg_day),
#                                                R2adj = R2ad_function(df$PP_kg_day_obs, df$PP_hill_kg_day)
#   )
#   alltime_GOF_SRP_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
#                                                 NSE = NSeff_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
#                                                 DV = DV_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
#                                                 KGE = hydroGOF::KGE(as.numeric(df$SRP_kg_day_obs), as.numeric(df$SRP_hill_kg_day)),
#                                                 MAE = hydroGOF::mae(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
#                                                 RMSE = rmse_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
#                                                 R2 = R2_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day),
#                                                 R2adj = R2ad_function(df$SRP_kg_day_obs, df$SRP_hill_kg_day)
#   )
#   alltime_GOF_Sediment_hillslope <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
#                                                      NSE = NSeff_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
#                                                      DV = DV_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
#                                                      KGE = hydroGOF::KGE(as.numeric(as.numeric(df$SSC_tonnes_day_obs)), as.numeric(df$SimSediment_tonnes_day_wshed)),
#                                                      MAE = hydroGOF::mae(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
#                                                      RMSE = rmse_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
#                                                      R2 = R2_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed),
#                                                      R2adj = R2ad_function(df$SSC_tonnes_day_obs, df$SimSediment_tonnes_day_wshed)
#   )
#   alltime_GOF_TP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
#                                           NSE = NSeff_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
#                                           DV = DV_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
#                                           KGE = hydroGOF::KGE(as.numeric(df$TP_kg_day_obs), as.numeric(df$TP_out_kg_day_outlet)),
#                                           MAE = hydroGOF::mae(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
#                                           RMSE = rmse_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
#                                           R2 = R2_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet),
#                                           R2adj = R2ad_function(df$TP_kg_day_obs, df$TP_out_kg_day_outlet)
#   )
#   alltime_GOF_PP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
#                                           NSE = NSeff_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
#                                           DV = DV_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
#                                           KGE = hydroGOF::KGE(as.numeric(df$PP_kg_day_obs), as.numeric(df$PP_out_kg_day_outlet)),
#                                           MAE = hydroGOF::mae(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
#                                           RMSE = rmse_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
#                                           R2 = R2_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet),
#                                           R2adj = R2ad_function(df$PP_kg_day_obs, df$PP_out_kg_day_outlet)
#   )
#   alltime_GOF_SRP_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
#                                            NSE = NSeff_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
#                                            DV = DV_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
#                                            KGE = hydroGOF::KGE(as.numeric(df$SRP_kg_day_obs), as.numeric(df$SRP_out_kg_day_outlet)),
#                                            MAE = hydroGOF::mae(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
#                                            RMSE = rmse_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
#                                            R2 = R2_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet),
#                                            R2adj = R2ad_function(df$SRP_kg_day_obs, df$SRP_out_kg_day_outlet)
#   )
#   alltime_GOF_Sediment_chan <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
#                                                 NSE = NSeff_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
#                                                 DV = DV_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
#                                                 KGE = hydroGOF::KGE(as.numeric(df$SSC_tonnes_day_obs), as.numeric(df$Sediment_tonnes_ebe)),
#                                                 MAE = hydroGOF::mae(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
#                                                 RMSE = rmse_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
#                                                 R2 = R2_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe),
#                                                 R2adj = R2ad_function(df$SSC_tonnes_day_obs, df$Sediment_tonnes_ebe)
#   )
#
#   a <- data.frame(alltime_GOF_SF, row.names = "Streamflow(mm)")
#   b <- data.frame(alltime_GOF_SWE, row.names = "SWE(mm)")
#   c <- data.frame(alltime_GOF_TP_hillslope, row.names = "TP_hillslope(kg/day)")
#   d <- data.frame(alltime_GOF_PP_hillslope, row.names = "PP_hillslope(kg/day)")
#   e <- data.frame(alltime_GOF_SRP_hillslope, row.names = "SRP_hillslope(kg/day)")
#   f<- data.frame(alltime_GOF_Sediment_hillslope, row.names = "Sediment_hillslope(tonnes/day)")
#   g <- data.frame(alltime_GOF_TP_chan, row.names = "TP_channel(kg/day)")
#   h <- data.frame(alltime_GOF_PP_chan, row.names = "PP_channel(kg/day)")
#   i <- data.frame(alltime_GOF_SRP_chan, row.names = "SRP_channel(kg/day)")
#   j <- data.frame(alltime_GOF_Sediment_chan, row.names = "Sediment_channel(tonnes/day)")
#   return(rbind(a,b,c,d,e,f,g,h,i,j))
# }
