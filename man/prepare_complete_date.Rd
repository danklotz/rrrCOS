% Please edit documentation in R/cook_data-GEN.R
\name{prepare_complete_date}
\alias{prepare_complete_date}
\title{Complete the date-formats with POSIXct or COSdate}
\usage{
prepare_complete_date(runoff_data = NULL, name_cosyear = "yyyy", 
    name_posix = "POSIXdate")
}
\arguments{
\item{runoff_data}{The data.frame, which contains the runoff information}

\item{name_cosyear}{string with the name of the `COSdate` year column}

\item{name_posi}{string with the name of the POSIXct column}
}
\value{
The new runoff data.frame with the added data-format.
}
\description{
Complete the data-formats of your data.frame `POSIXct` and/or `COSdate`
}
