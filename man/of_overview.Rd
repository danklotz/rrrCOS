% Please edit documentation in R/get_main_of-GEN.R, R/main_of-GEN.R, R/of-GEN.R
\name{barplot_main_of}
\alias{barplot_main_of}
\alias{main_of_barplot}
\alias{main_of_rasterplot}
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
\alias{rasterplot_main_of}
\title{Barplot for the Main Objective Function Values}
\usage{
barplot_main_of(runoff_data)

rasterplot_main_of(runoff_data)

main_of_barplot(runoff_data)

main_of_rasterplot(runoff_data)

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
Barplot for the Main Objective Function Values

Barplot for the Main Objective Function Values

Barplot for the Main Objective Function Values

Barplot for the Main Objective Function Values

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
