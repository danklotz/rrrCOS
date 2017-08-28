% Please edit documentation in R/time_aggregation-GEN.R
\name{aggregate_time}
\alias{aggregate_time}
\title{Time Aggregation}
\usage{
aggregate_time(cosdata, key = "mm", .funs = base::mean, opts = coscos::viscos_options())
}
\arguments{
\item{cosdata}{The strictly defined data format (\code{cosdata}) used 
within \pkg{viscos}}

\item{aggregation}{A string that defines the resolution of the aggregation.}
}
\description{
Aggregates the COSERO data.frame (\code{cosdata}) according to the
timely resolution defined via \code{aggregation}. Possible
resolution-choices are \code{'yyyy'} - year, \code{'mm'} - month and
\code{'dd'} - day and combinations thereof.
}
