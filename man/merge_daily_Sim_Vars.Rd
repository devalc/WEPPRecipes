% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_daily_Sim_Vars.R
\name{merge_daily_Sim_Vars}
\alias{merge_daily_Sim_Vars}
\title{Merges all the daily simulated variables into one df}
\usage{
merge_daily_Sim_Vars(totalwatsed_df, chanwb_df, ebe_df, sim_P_df)
}
\arguments{
\item{totalwatsed_df}{Name of the totalwatsed dataframe}

\item{chanwb_df}{Name of the channel water balance dataframe}

\item{ebe_df}{Name of the event by event dataframe}

\item{sim_P_df}{Name of the dataframe containing Phosphorus data}
}
\value{
A merged dataframe with all variables also in the depth units
}
\description{
This function reads the totalwatsed, channel water balance
and event by event dataframes and returns
a merged dataframe of simulated variables in depth units
}
