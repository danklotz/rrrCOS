% Please edit documentation in R/explore_basins_with_of-GEN.R, R/exploring_of-GEN.R
\name{explore_runoff_with_ofun}
\alias{explore_runoff_with_ofun}
\title{explore runoff_data with Objective Functions}
\usage{
explore_runoff_with_ofun(runoff_data)

explore_runoff_with_ofun(runoff_data)
}
\arguments{
\item{d_xts}{runoff_data formatted as time series}

\item{d_xts}{runoff_data formatted as time series}
}
\description{
Runs a Shiny App which can be used to get an overview of a runoff_data time
series object.

Runs a Shiny App which can be used to get an overview of a runoff_data time
series object.
}
\examples{
# get example data, clean it and explore the model performance
d_runoff <- get_runoff_example()
explore_runoff_with_ofun(d_runoff)
# get example data, clean it and explore the model performance
d_runoff <- get_runoff_example()
explore_runoff_with_ofun(d_runoff)
}
