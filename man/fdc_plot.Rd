% Please edit documentation in R/flow_duration_curves-GEN.R
\name{fdc_plot}
\alias{fdc_plot}
\title{Plot Flow Duration Curves}
\usage{
fdc_plot(cos_data, log_y = TRUE, log_x = FALSE, ...)
}
\description{
Plots the flow duration curves (fdc) for `cos_data`. 
The function uses `ggplot` to so and facets the different basins into 
separate subplots. Each subplot shows the fdc of the \eqn{o}-data and 
the \eqn{s}-data.
}
