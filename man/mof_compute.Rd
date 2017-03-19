% Please edit documentation in R/mof-GEN.R
\name{mof_compute}
\alias{mof_compute}
\title{Get basic objective function for cos_data}
\usage{
mof_compute(cos_data)
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
