% Please edit documentation in R/time_aggregation-GEN.R
\name{aggregate_time}
\alias{aggregate_time}
\title{Time Aggregation}
\usage{
aggregate_time(cos_data, aggregation = "mm")
}
\arguments{
\item{cos_data}{the COSERO data.frame as used within visCOS}

\item{aggregation}{string that defines the resolution of the aggregation.}
}
\description{
Aggregates the cosero data.frame (\code{cos_data}) according to the 
timely resolution defined via \code{aggregation}. Possible 
resolution-choices are 'yyyy' - year, 'mm' - month and 'dd' - day and 
combinations thereof.
}
