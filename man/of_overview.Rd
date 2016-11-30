% Please edit documentation in R/of-GEN.R
\name{of_overview}
\alias{of_cor}
\alias{of_invert_nse}
\alias{of_kge}
\alias{of_nse}
\alias{of_overview}
\alias{of_pbias}
\alias{of_rmeans}
\alias{of_rmse}
\alias{of_rsd}
\alias{of_ve}
\title{Objective Functions}
\usage{
of_rmse(x, y)

of_pbias(x, y)

of_nse(x, y)

of_invert_nse(x, y)

of_kge(x, y)

of_cor(x, y)

of_rsd(x, y)

of_rmeans(x, y)

of_ve(x, y)
}
\arguments{
\item{x}{observation}

\item{y}{simulations}
}
\description{
Different objective Functions, provided by visCOS.

Root Mean Sqaured Error

Percentage Bias

Nash-Sutcliffe Efficiency

Inverted Nash-Sutcliffe Efficiency

Kling-Gupta Efficiency

Correlation

Ratio of Standard Deviations

Ratio of Means

Volumetric Efficiency
}
