#' Reads and processes totalwatsed file
#'
#'This function reads the totalwatsed file and returns
#'a dataframe that also includes all the simulated
#'variables in depth units
#'
#'
#' @param totalwatsed_path A string pointing to the location of the totalwatsed file
#' @param init_reservoir_vol_mm Initial reservoir volumne in mm
#' @param init_Baseflow_val_mm Initial baseflow value mm
#' @param Baseflow_coeff Baseflow coefficient
#' @param Aquifer_coeff Aquifer coefficient
#' @return A totalwatsed dataframe along with all the
#' simulated variables in the depth units
#' @export
#'
#'

process_totalwatsed <- function(totalwatsed_path, init_reservoir_vol_mm,
                                     init_Baseflow_val_mm,
                                     Baseflow_coeff,
                                     Aquifer_coeff){
  ## read water and sediment data
  totalwatsed <- read.table(totalwatsed_path,header = F)


  ### set names of the dataframes
  colnames(totalwatsed) <- c("DOY_wshed","Y_wshed", "Area_m2_wshed", "P_m3_wshed" ,"RM_m3_wshed", "T_m3_wshed", "E_m3_wshed", "Perc_m3_wshed",
                             "Runoff_m3_wshed", "Lateral_m3_wshed", "Storage_m3_wshed", "Sed_Det_kg_wshed",	"Sed_Dep_kg_wshed",
                             "Sed_Del_kg_wshed",	"Class_1_wshed",	"Class_2_wshed",	"Class_3_wshed",	"Class_4_wshed",	"Class_5_wshed")

  #convert volume to depth units
  totalwatsed <- totalwatsed %>% dplyr::mutate_at(c("P_m3_wshed" ,"RM_m3_wshed", "T_m3_wshed", "E_m3_wshed", "Perc_m3_wshed",
                                                    "Runoff_m3_wshed", "Lateral_m3_wshed", "Storage_m3_wshed"),
                                                  funs(to_mm = ./Area_m2_wshed * 1000)) %>%
    dplyr::mutate(ET_mm_wshed = E_m3_wshed_to_mm + T_m3_wshed_to_mm,
                  Sed_Del_tonnes_wshed = cumsum(Sed_Del_kg_wshed/1000)) %>%
    dplyr::mutate(Sed_Del_tonnes_ha_wshed = Sed_Del_tonnes_wshed / Area_m2_wshed * 10000,
                  originDate = as.Date(paste0(Y_wshed, "-01-01"),tz = "UTC") - days(1),
                  Date = as.Date(DOY_wshed, origin = originDate, tz = "UTC"),
                  WY = get_waterYear(Date)) %>% select(-originDate) %>%
    select(DOY_wshed, Y_wshed, Date, WY, everything())


  totalwatsed$Resvol_mm_wshed[1] <- init_reservoir_vol_mm
  totalwatsed$Baseflow_mm_wshed[1] <- init_Baseflow_val_mm
  totalwatsed$SimSWE_mm_wshed[1] <- 0

  for (i in 2:length(totalwatsed$P_m3_wshed))
  {
    totalwatsed$AquiferLoss_mm_wshed[i] <- totalwatsed$Resvol_mm_wshed[i-1] * Aquifer_coeff
    totalwatsed$Resvol_mm_wshed[i] <- totalwatsed$Resvol_mm_wshed[i-1]+ totalwatsed$Perc_m3_wshed_to_mm[i] - totalwatsed$AquiferLoss_mm_wshed[i] -totalwatsed$Baseflow_mm_wshed[i-1]
    totalwatsed$Baseflow_mm_wshed[i] <- totalwatsed$Resvol_mm_wshed[i]*Baseflow_coeff
    totalwatsed$SimSWE_mm_wshed[i] <- totalwatsed$SimSWE_mm_wshed[i-1] + totalwatsed$P_m3_wshed_to_mm[i] - totalwatsed$RM_m3_wshed_to_mm[i]
  }

  totalwatsed <- totalwatsed %>% mutate(SimStreamflow_mm_wshed = Runoff_m3_wshed_to_mm + Lateral_m3_wshed_to_mm + Baseflow_mm_wshed,
                                        PercRunoff_ofStreamflow_mm_wshed = Runoff_m3_wshed_to_mm/SimStreamflow_mm_wshed *100,
                                        PercLateral_ofStreamflow_mm_wshed = Lateral_m3_wshed_to_mm/SimStreamflow_mm_wshed *100,
                                        PercBaseflow_ofStreamflow_mm_wshed = Baseflow_mm_wshed/SimStreamflow_mm_wshed *100,
                                        SimSediment_tonnes_day_wshed = Sed_Del_kg_wshed/1000

  )

  return(totalwatsed)

}
