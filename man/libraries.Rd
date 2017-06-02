% Please edit documentation in R/cook_data-GEN.R
\name{libraries}
\alias{libraries}
\title{Loading many libraries at once}
\usage{
libraries(x, ...)
}
\arguments{
\item{x}{character vector. Name(s) of the libraries that are loaded/installed.}

\item{...}{Arguments passed to \code{\link{require}} and \code{\link{install.packages}}}
}
\value{
Returns nothing but gives a warning if it cannot load/install a library/package
}
\description{
This function tries to load more than one package at once. If any of these packages is not installed it tries to istall them and load them afterward.
}
\examples{
# loading xts
libraries("xts")
libraries(c("xts", "shiny"))
}
\author{
Simon Frey
}
\seealso{
\code{\link{require}}, \code{\link{library}}, \code{\link{install.packages}}
}
