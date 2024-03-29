% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ctrl_chart.R
\name{plot_ctrl_chart}
\alias{plot_ctrl_chart}
\title{Plot control chart}
\usage{
plot_ctrl_chart(df, plot_center_line = T, fmt_x_quarterly = F)
}
\arguments{
\item{df}{A data frame in wide format, created by the structure_data function}

\item{plot_center_line}{If TRUE, the center line will be added to the plot}

\item{quarterly}{If TRUE, the x axis will be formatted as quarterly; if FALSE, x axis is formatted monthly. Default value is FALSE (formatted monthly).}
}
\description{
Plot control chart
}
