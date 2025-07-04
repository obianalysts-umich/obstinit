% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p4p_measures_complaince.R
\name{prop_births_mtg_COMFORT_compliance}
\alias{prop_births_mtg_COMFORT_compliance}
\title{Function to calculate compliance with COMFORT guideline by mode of delivery}
\usage{
prop_births_mtg_COMFORT_compliance(
  obi_dt,
  ...,
  limit_to_2024 = T,
  by_site = lifecycle::deprecated(),
  max_OME_vag_lac_val = lifecycle::deprecated()
)
}
\arguments{
\item{obi_dt}{A data frame containing the necessary columns; current RDS dataframe is sufficient.}

\item{...}{Grouping arguments. By default includes opioid_group; other variables can be added as needed.}

\item{limit_to_2024}{Logical value indicating whether to filter the data to cases with infant dob year ≥ 2024; default is set to T}

\item{by_site}{Should the output dataframe be grouped by site? Defaults to true for ease of P4P scoring. DEPRECTAED OCTOBER 2024.}

\item{max_OME_vag_lac_val}{COMFORT max recommended value for vaginal births with 3rd/4th degree laceration. Default is 75 for now, learned in mid-September 2024 that final COMFORT recommendation will by 37.5 (38 for OBI purposes)}
}
\value{
A data frame with the proportion of eligible births with opioid prescribing consistent with the COMFORT guideline by mode of delivery
}
\description{
This function takes OBI data as input and calculates what proportion of eligible births were compliant with the COMFORT guideline for their particular mode of delivery
Modes of delivery include vaginal with no laceration, vaginal with 3rd/4th degree laceration, and cesarean section
Max acceptable OME is 0 for vaginal births with no laceration, 75 for vaginal births with 3rd/4th degree laceration, and 113 for cesarean births
}
\seealso{
Other {2024 P4P measures}: 
\code{\link{average_days_to_submit}()},
\code{\link{prop_scheduled_non_opioid_meds}()},
\code{\link{pv_email_submission_rate}()},
\code{\link{race_ethnicity_measure}()}
}
\concept{{2024 P4P measures}}
