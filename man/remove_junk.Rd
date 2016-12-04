% Please edit documentation in R/cook_data-GEN.R
\name{remove_junk}
\alias{remove_junk}
\title{removes chunk in runoff_data}
\usage{
remove_junk(runoff_data)
}
\arguments{
\item{runoff_data}{data.frame object containing at least COSdate,
and two data rows defined in viscos_options}
}
\value{
data.frame object without the chunk
}
\description{
Removes all columns which are not foreseen (see: viscos_options) from
runoff data
}
