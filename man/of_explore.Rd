% Please edit documentation in R/of_explore-GEN.R
\name{of_explore}
\alias{of_explore}
\title{Explore with Objective Functions}
\usage{
of_explore(cos_data, of_list = list(nse = of_nse, kge = of_kge, p_bias = of_p_bias, 
    r = of_cor), start_date = NULL, end_date = NULL)
}
\description{
Runs a Shiny Gadget which can be used to get an overview of a cos_data time
series object.
}
\examples{
# get example data, explore the model performance
cos_data <- get_viscos_example()
of_explore(cos_data)
}
