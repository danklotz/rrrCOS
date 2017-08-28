% Please edit documentation in R/of-GEN.R
\name{of_compute}
\alias{of_compute}
\title{Get basic objective function for cos_data}
\usage{
of_compute(cosdata, d_metrics = list(nse = d_nse, kge = d_kge, pbias = d_pbias, 
    corr = d_cor))
}
\arguments{
\item{cos_data}{cos_data data.frame.}
}
\value{
list of basic objective function evaluated for the different
hydrological years and over the whole timespan.
}
\description{
Calculate basic objective functions(NSE, KGE, percentage BIAS, Correlation)
for every basin and the chosen periods.
}
