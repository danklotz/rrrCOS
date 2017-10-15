% Please edit documentation in R/of-GEN.R
\name{judge_compute}
\alias{judge_compute}
\title{Get basic objective function for cos_data}
\usage{
judge_compute(cosdata, d_metrics = list(nse = d_nse, kge = d_kge, pbias = d_pbias, 
    corr = d_cor), opts = coscos::viscos_options())
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
