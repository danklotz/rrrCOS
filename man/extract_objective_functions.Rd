% Please edit documentation in R/exploring_of-GEN.R
\name{extract_objective_functions}
\alias{extract_objective_functions}
\title{Get basic objective function for runoff_data}
\usage{
extract_objective_functions(runoff_data)
}
\arguments{
\item{runoff_data}{runoff_data data.frame (see:xxx).}
}
\value{
list of basic objective function evaluated for the different
hydrological years and over the whole timespan.
}
\description{
Calculate basic objective functions
(NSE, KGE, percentage BIAS, Correlation (see: xxx)) for
every basin and the chosen periods
}
