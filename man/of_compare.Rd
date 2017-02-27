% Please edit documentation in R/of_compare-GEN.R
\name{of_compare}
\alias{of_compare}
\title{Explore with Objective Functions}
\usage{
of_compare(d1, d2 = NULL, of_list = list(nse = of_nse, kge = of_kge, p_bias = of_p_bias, 
    r = of_cor), start_date = NULL, end_date = NULL)
}
\description{
Runs a Shiny Gadget which can be used to interactively explore two 
`cos_data` objects in terms of their respective objective functions.
}
\examples{
# get example data, explore the model performance
cos_data <- get_viscos_example()
explore_cos_data(cos_data)
}
