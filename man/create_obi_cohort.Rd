% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_90_day_lock_dt.R, R/create_obi_cohort.R
\name{create_90_day_lock_dt}
\alias{create_90_day_lock_dt}
\alias{create_obi_cohort}
\title{Create OBI case lock dates}
\usage{
create_90_day_lock_dt()

create_obi_cohort(df)
}
\arguments{
\item{df}{Data frame -- usually this is the data frame read into R using data.table::fread}
}
\description{
Create OBI case lock dates

Create OBI cohort of locked, complete cases for most analyses
}
