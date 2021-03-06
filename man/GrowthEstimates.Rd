% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Other_Functions.R
\name{GrowthEstimates}
\alias{GrowthEstimates}
\title{GrowthEstimates}
\usage{
GrowthEstimates(data, cols, start = 2000, end = 2010)
}
\arguments{
\item{data}{a dataset with YEAR, ID columbs, as well as specified population columns}

\item{cols}{a string or list of strings naming columns to make estimates on}

\item{start}{a year for which you have data from which to start making estimates}

\item{end}{a year for which you have data until which you make estimates}
}
\value{
A dataset containing population estimates--expotential growth--between the given years for the given population columns
}
\description{
Given a dataset and two years, creates a new dataset containing population estimates for years between those two days
}
