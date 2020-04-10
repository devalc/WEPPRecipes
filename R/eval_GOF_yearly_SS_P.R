#' Returns a goodness of fit statistics for WY Sediment, Phosphorus
#'
#'
#'  Takes in a dataframe containing both observed and simulated variables
#'  (of identical length) and returns NSE, DV, KGE, MAE, RMSE, R2 and R2adj
#'  for each variable namely.
#'
#'  Example variable nomenclature: Observed X --> X_Load_tonnes (i.e. for total phosphorus ---> TP_Load_tonnes)
#'                                 Simulated X --> SimX_tonnes (i.e. for total phosphorus ---> SimTP_tonnes)
#' @param df dataframe containing observed and sumulated data for Phosphorus and sediment.
#' @return Goodness of Fit between observed and simulated data
#'
#' @export
#'
#'
eval_GOF_yearly_SS_P <- function(df){

annual_GOF_TP_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$TP_Load_tonnes, df$SimTP_tonnes),
                                                   NSE = NSeff_function(df$TP_Load_tonnes, df$SimTP_tonnes),
                                                   DV = DV_function(df$TP_Load_tonnes, df$SimTP_tonnes),
                                                   KGE = hydroGOF::KGE(as.numeric(df$TP_Load_tonnes), as.numeric(df$SimTP_tonnes)),
                                                   MAE = hydroGOF::mae(df$TP_Load_tonnes, df$SimTP_tonnes),
                                                   RMSE = rmse_function(df$TP_Load_tonnes, df$SimTP_tonnes),
                                                   R2 = R2_function(df$TP_Load_tonnes, df$SimTP_tonnes),
                                                   R2adj = R2ad_function(df$TP_Load_tonnes, df$SimTP_tonnes)
)
annual_GOF_PP_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$PP_Load_tonnes, df$SimPP_tonnes),
                                                   NSE = NSeff_function(df$PP_Load_tonnes, df$SimPP_tonnes),
                                                   DV = DV_function(df$PP_Load_tonnes, df$SimPP_tonnes),
                                                   KGE = hydroGOF::KGE(as.numeric(df$PP_Load_tonnes), as.numeric(df$SimPP_tonnes)),
                                                   MAE = hydroGOF::mae(df$PP_Load_tonnes, df$SimPP_tonnes),
                                                   RMSE = rmse_function(df$PP_Load_tonnes, df$SimPP_tonnes),
                                                   R2 = R2_function(df$PP_Load_tonnes, df$SimPP_tonnes),
                                                   R2adj = R2ad_function(df$PP_Load_tonnes, df$SimPP_tonnes)
)
annual_GOF_SRP_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SRP_Load_tonnes, df$SimSRP_tonnes),
                                                    NSE = NSeff_function(df$SRP_Load_tonnes, df$SimSRP_tonnes),
                                                    DV = DV_function(df$SRP_Load_tonnes, df$SimSRP_tonnes),
                                                    KGE = hydroGOF::KGE(as.numeric(df$SRP_Load_tonnes), as.numeric(df$SimSRP_tonnes)),
                                                    MAE = hydroGOF::mae(df$SRP_Load_tonnes, df$SimSRP_tonnes),
                                                    RMSE = rmse_function(df$SRP_Load_tonnes, df$SimSRP_tonnes),
                                                    R2 = R2_function(df$SRP_Load_tonnes, df$SimSRP_tonnes),
                                                    R2adj = R2ad_function(df$SRP_Load_tonnes, df$SimSRP_tonnes)
)
annual_GOF_Sediment_watershed <- df %>% dplyr::summarise(nPairs = nPairs_function(df$SS_Load_tonnes, df$SimSed_tonnes),
                                                         NSE = NSeff_function(df$SS_Load_tonnes, df$SimSed_tonnes),
                                                         DV = DV_function(df$SS_Load_tonnes, df$SimSed_tonnes),
                                                         KGE = hydroGOF::KGE(as.numeric(as.numeric(df$SS_Load_tonnes)), as.numeric(df$SimSed_tonnes)),
                                                         MAE = hydroGOF::mae(df$SS_Load_tonnes, df$SimSed_tonnes),
                                                         RMSE = rmse_function(df$SS_Load_tonnes, df$SimSed_tonnes),
                                                         R2 = R2_function(df$SS_Load_tonnes, df$SimSed_tonnes),
                                                         R2adj = R2ad_function(df$SS_Load_tonnes, df$SimSed_tonnes)
)

a <- data.frame(annual_GOF_TP_watershed, row.names = "Total Phosphorus(Tonnes)")
b <- data.frame(annual_GOF_PP_watershed, row.names = "Particulate Phosphorus(Tonnes)")
c <- data.frame(annual_GOF_SRP_watershed, row.names = "Soluble Reactive Phosphorus(Tonnes)")
d <- data.frame(annual_GOF_Sediment_watershed, row.names = "Sediments(Tonnes)")
return(rbind(a,b,c,d))
}
