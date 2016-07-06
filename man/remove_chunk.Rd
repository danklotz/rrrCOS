% Please edit documentation in R/cook_data-GEN.R
\name{remove_chunk}
\alias{remove_chunk}
\title{removes chunk in runoff_data}
\usage{
remove_chunk(runoff_data)
}
\arguments{
\item{runoff_data}{data.frame object containing at lesast COSdate, 
Qsim and Qobs (see: xxx)}
}
\value{
data.frame object withouth the chunk
}
\description{
Remove all collumns which are not foreseen (see: viscos_options) from 
runoff data
}
