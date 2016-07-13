% Please edit documentation in R/cook_data-GEN.R
\name{remove_chunk}
\alias{remove_chunk}
\title{removes chunk in runoff_data}
\usage{
remove_chunk(runoff_data)
}
\arguments{
\item{runoff_data}{data.frame object containing at least COSdate,
Qsim and Qobs (see: xxx)}
}
\value{
data.frame object without the chunk
}
\description{
Remove all columns which are not foreseen (see: viscos_options) from
runoff data
}
