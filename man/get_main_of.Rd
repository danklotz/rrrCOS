% Please edit documentation in R/calculating_of-GEN.R
\name{get_main_of}
\alias{get_main_of}
\title{Get basic objective function for runoff_data}
\usage{
get_main_of(runoff_data)
}
\arguments{
\item{runoff_data}{runoff_data data.frame.}
}
\value{
list of basic objective function evaluated for the different
hydrological years and over the whole timespan.
}
\description{
Calculate basic objective functions(NSE, KGE, percentage BIAS, Correlation) 
for every basin and the chosen periods.
}
