% Please edit documentation in R/explore_basins_with_of-GEN.R
\name{explore_runoff_with_of}
\alias{explore_runoff_with_of}
\title{explore runoff_data with Objective Functions}
\usage{
explore_runoff_with_of(runoff_data)
}
\arguments{
\item{d_xts}{runoff_data formatted as time series}
}
\description{
Runs a Shiny App which can be used to get an overview of a runoff_data time
series object.
}
\examples{
# get example data, clean it and explore the model performance
d_runoff <- get_runoff_example()
explore_runoff_with_of(d_runoff)
}
