% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_chanwb.R
\name{process_chanwb}
\alias{process_chanwb}
\title{Reads and processes Channel water balance file}
\usage{
process_chanwb(chanwb_path, Wshed_Area_m2)
}
\arguments{
\item{chanwb_path}{A string pointing to the location of
the channel water balance file}

\item{Wshed_Area_m2}{Area of the simulated watershed in m2}
}
\value{
A Channel water balace dataframe including streamflow in dpeth units
}
\description{
This function reads the channel water balance file and returns
a dataframe of simulated variables in depth units
}
