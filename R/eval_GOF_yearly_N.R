#' Returns a goodness of fit statistics for WY Nitrogen
#'
#'
#'  Takes in a dataframe containing both observed and simulated variables
#'  (of identical length) and returns NSE, DV, KGE, MAE, RMSE, R2 and R2adj
#'  for each variable namely.
#'
#'  Example variable nomenclature: Observed X --> X_Load_tonnes (i.e. for total nitrogen ---> TN_Load_tonnes)
#'                                 Simulated X --> SimX_tonnes (i.e. for total nitrogen ---> SimTN_tonnes)
#' @param df dataframe containing observed and sumulated data for Nitrogen.
#' @return Goodness of Fit between observed and simulated data
#'
#' @export
#'
#'
eval_GOF_yearly_N <- function(df){

  annual_GOF_TN_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$TN_Load_tonnes, df$SimTN_tonnes),
                                                     NSE = NSeff_function(df$TN_Load_tonnes, df$SimTN_tonnes),
                                                     DV = DV_function(df$TN_Load_tonnes, df$SimTN_tonnes),
                                                     KGE = hydroGOF::KGE(as.numeric(df$TN_Load_tonnes), as.numeric(df$SimTN_tonnes)),
                                                     MAE = hydroGOF::mae(df$TN_Load_tonnes, df$SimTN_tonnes),
                                                     RMSE = rmse_function(df$TN_Load_tonnes, df$SimTN_tonnes),
                                                     R2 = R2_function(df$TN_Load_tonnes, df$SimTN_tonnes),
                                                     R2adj = R2ad_function(df$TN_Load_tonnes, df$SimTN_tonnes)
  )
  annual_GOF_TKN_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$TKN_Load_tonnes, df$SimTKN_tonnes),
                                                      NSE = NSeff_function(df$TKN_Load_tonnes, df$SimTKN_tonnes),
                                                      DV = DV_function(df$TKN_Load_tonnes, df$SimTKN_tonnes),
                                                      KGE = hydroGOF::KGE(as.numeric(df$TKN_Load_tonnes), as.numeric(df$SimTKN_tonnes)),
                                                      MAE = hydroGOF::mae(df$TKN_Load_tonnes, df$SimTKN_tonnes),
                                                      RMSE = rmse_function(df$TKN_Load_tonnes, df$SimTKN_tonnes),
                                                      R2 = R2_function(df$TKN_Load_tonnes, df$SimTKN_tonnes),
                                                      R2adj = R2ad_function(df$TKN_Load_tonnes, df$SimTKN_tonnes)
  )
  annual_GOF_NO3_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$NO3_Load_tonnes, df$SimNO3_tonnes),
                                                      NSE = NSeff_function(df$NO3_Load_tonnes, df$SimNO3_tonnes),
                                                      DV = DV_function(df$NO3_Load_tonnes, df$SimNO3_tonnes),
                                                      KGE = hydroGOF::KGE(as.numeric(df$NO3_Load_tonnes), as.numeric(df$SimNO3_tonnes)),
                                                      MAE = hydroGOF::mae(df$NO3_Load_tonnes, df$SimNO3_tonnes),
                                                      RMSE = rmse_function(df$NO3_Load_tonnes, df$SimNO3_tonnes),
                                                      R2 = R2_function(df$NO3_Load_tonnes, df$SimNO3_tonnes),
                                                      R2adj = R2ad_function(df$NO3_Load_tonnes, df$SimNO3_tonnes)
  )
  annual_GOF_NH4_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$NH4_Load_tonnes, df$SimNH4_tonnes),
                                                      NSE = NSeff_function(df$NH4_Load_tonnes, df$SimNH4_tonnes),
                                                      DV = DV_function(df$NH4_Load_tonnes, df$SimNH4_tonnes),
                                                      KGE = hydroGOF::KGE(as.numeric(df$NH4_Load_tonnes), as.numeric(df$SimNH4_tonnes)),
                                                      MAE = hydroGOF::mae(df$NH4_Load_tonnes, df$SimNH4_tonnes),
                                                      RMSE = rmse_function(df$NH4_Load_tonnes, df$SimNH4_tonnes),
                                                      R2 = R2_function(df$NH4_Load_tonnes, df$SimNH4_tonnes),
                                                      R2adj = R2ad_function(df$NH4_Load_tonnes, df$SimNH4_tonnes)
  )

  a <- data.frame(annual_GOF_TN_watershed, row.names = "Total Nitrogen(Tonnes)")
  b <- data.frame(annual_GOF_TKN_watershed, row.names = "TKN(Tonnes)")
  c <- data.frame(annual_GOF_NO3_watershed, row.names = "NO3(Tonnes)")
  d <- data.frame(annual_GOF_NH4_watershed, row.names = "NH4(Tonnes)")
  return(rbind(a,b,c,d))
}
