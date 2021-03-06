% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_P_at_outlet.R
\name{est_P_at_outlet}
\alias{est_P_at_outlet}
\title{Estimates Phosphorus load, phosphorus in runoff,
baseflow and lateralflow}
\usage{
est_P_at_outlet(P_outlet_df, Wshed_area_m2, par)
}
\arguments{
\item{P_outlet_df}{Dataframe created by the create_var_df_P_outlet function}

\item{Wshed_area_m2}{Area of the watershed in m2}

\item{par}{Vector containg parameter values (mg/l) in the specific order-
P_conc_Runoff, P_conc_Lateral, P_conc_Baseflow, P_conc_Sediment}
}
\value{
A dataframe containg estimated phosporus load,
 phosphorus in runoff, baseflow and lateral flow at the outlet
}
\description{
This function reads in totalwatsed file and a vector containing
paramter values, estimates Phosophorus and return
a dataframe
}
