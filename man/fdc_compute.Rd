% Please edit documentation in R/flow_duration_curves-GEN.R
\name{fdc_compute}
\alias{fdc_compute}
\title{Compute Flow Duration Curves}
\usage{
fdc_compute(cos_data)
}
\arguments{
\item{cos_data}{A data.frame with columns as used throughout visCOS}
}
\description{
Computes the flow duration curves (fdc) for the `cos_data` data.frame. 
The calculations are adapted from the method used within the hydroTSM package.
}
