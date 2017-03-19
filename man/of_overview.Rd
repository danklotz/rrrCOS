% Please edit documentation in R/mof-GEN.R, R/of-GEN.R
\name{mof_barplot}
\alias{mof_barplot}
\alias{mof_rasterplot}
\alias{of_cor}
\alias{of_invert_nse}
\alias{of_kge}
\alias{of_nse}
\alias{of_overview}
\alias{of_p_bias}
\alias{of_rmeans}
\alias{of_rmse}
\alias{of_rsd}
\alias{of_ve}
\title{Bar plot for the Main Objective Function Values}
\usage{
mof_barplot(cos_data)

mof_rasterplot(cos_data)

of_nse(o, s)

of_kge(o, s)

of_p_bias(o, s)

of_cor(o, s)

of_rmse(o, s)

of_invert_nse(o, s)

of_rsd(o, s)

of_rmeans(o, s)

of_ve(o, s)
}
\arguments{
\item{o}{The reference data or observations (o_data)}

\item{s}{The created data or the simulations (s_data)}
}
\description{
Bar plot for the Main Objective Function Values

Bar plot for the Main Objective Function Values

Different objective Functions, provided by visCOS. A detailed description
of each of the provided objective function is provided in the respective
vignette

Nash-Sutcliffe Efficiency

Kling-Gupta Efficiency

Percentage Bias

Correlation

Root Mean Sqaured Error

Inverted Nash-Sutcliffe Efficiency

Ratio of Standard Deviations

Ratio of Means

Volumetric Efficiency
}
