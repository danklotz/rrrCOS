% Please edit documentation in R/exploring_of-GEN.R
\name{serve.runoff_with_ofun}
\alias{serve.runoff_with_ofun}
\title{serve with ofun}
\usage{
serve.runoff_with_ofun(runoff_data)
}
\arguments{
\item{d_xts}{runoff_data formatted as time series}
}
\description{
Runs a Shiny App which can be used to get an overview of a runoff_data time series object.
Explore the runoff_data with a little [shiny](http://shiny.rstudio.com/) App.
}
\examples{
# get example data, clean it and explore the model performance
d_runoff <- prepare.remove_chunk(pour.runoff_example())
serve.runoff_with_ofun(d_runoff)
}
