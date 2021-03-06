% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_totalwatsed.R
\name{process_totalwatsed}
\alias{process_totalwatsed}
\title{Reads and processes totalwatsed file}
\usage{
process_totalwatsed(
  totalwatsed_path,
  init_reservoir_vol_mm,
  init_Baseflow_val_mm,
  Baseflow_coeff,
  Aquifer_coeff
)
}
\arguments{
\item{totalwatsed_path}{A string pointing to the location of the totalwatsed file}

\item{init_reservoir_vol_mm}{Initial reservoir volumne in mm}

\item{init_Baseflow_val_mm}{Initial baseflow value mm}

\item{Baseflow_coeff}{Baseflow coefficient}

\item{Aquifer_coeff}{Aquifer coefficient}
}
\value{
A totalwatsed dataframe along with all the
simulated variables in the depth units
}
\description{
This function reads the totalwatsed file and returns
a dataframe that also includes all the simulated
variables in depth units
}
