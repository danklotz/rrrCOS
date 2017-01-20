% Please edit documentation in R/cook_data-GEN.R
\name{remove_junk}
\alias{remove_junk}
\title{removes junk in cos_data}
\usage{
remove_junk(cos_data)
}
\arguments{
\item{cos_data}{The cos_data data.frame (see vignette for info)}
}
\value{
data.frame object without the chunk
}
\description{
Removes all columns which are not foreseen (see: viscos_options) from
runoff data
}
