% Please edit documentation in R/cook_data-GEN.R
\name{mark_periods}
\alias{mark_periods}
\title{Mark Periods}
\usage{
mark_periods(cos_data, start_month = 10, end_month = 9)
}
\arguments{
\item{cos_data}{a data.frame that contains the runoff information.}
}
\value{
`cos_data` with an aditonal column with the marked periods.
}
\description{
Compute/Mark the periods within cos_data. The marking uses a monthly
resolution, which are defined by the integers `start_month` and
`end_month`.
}
