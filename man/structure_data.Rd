% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structure_data.R
\name{structure_data}
\alias{structure_data}
\title{Structure OBI data for plotting control charts}
\usage{
structure_data(
  df,
  date_var,
  num_var,
  den_var,
  date_gran = "month",
  nsigmas = 3,
  long = F,
  increase_is_bad = T,
  for_highchart = F
)
}
\arguments{
\item{df}{A data frame}

\item{date_var}{The date variable to be used for grouping; usually infant_dob_dt;
Make use this variable is in a date format, for example: lubridate::dmy_hms(infant_dob_dt)}

\item{num_var}{The variable to be summarized as the numerator of the rate we're interested in calculating - should be binary 0 1}

\item{den_var}{The variable to be summarized as the denominator of the rate we're interested in calculating - should be binary 0 1}

\item{date_gran}{The granularity of dates we want to use for our control chart; "month" or "quarter"}

\item{nsigmas}{a numeric value specifying the number of sigmas to use for computing control limits.}

\item{long}{Whether to pivot the data to long format - default is T as this is the data structure needed for ggplot2}

\item{increase_is_bad}{If TRUE, this is a trend that would ideally be decreasing over time; if FALSE, ideally increasing}

\item{for_highchart}{If TRUE, multiplies all rates by 100 for highchart}
}
\description{
Structure OBI data for plotting control charts
}
