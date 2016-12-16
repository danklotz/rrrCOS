% Please edit documentation in R/explore_runoff-GEN.R
\name{explore_cos_data}
\alias{explore_cos_data}
\title{explore cos_data with Objective Functions}
\usage{
explore_cos_data(cos_data, of_list = list(nse = of_nse, kge = of_kge, 
    p_bias = of_p_bias, r = of_cor), start_date = NULL, end_date = NULL)
}
\arguments{
\item{d_xts}{cos_data formatted as time series}
}
\description{
Runs a Shiny App which can be used to get an overview of a cos_data time
series object.
}
\examples{
# get example data, explore the model performance
cos_data <- get_cos_data_example()
explore_cos_data(cos_data)
}
