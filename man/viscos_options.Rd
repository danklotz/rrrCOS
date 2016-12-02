% Please edit documentation in R/viscos_options-GEN.R
\docType{data}
\name{viscos_options}
\alias{viscos_options}
\title{visCOS global options}
\format{An object of class \code{GlobalOptionsFun} of length 1.}
\usage{
viscos_options
}
\description{
Get and set the global options of visCOS
}
\details{
These are the options you can adapt by executing the function 
(default values)
\preformatted{
  viscos_options(
   # data.frame column names
      name_o = "qobs", # name of the first time-series data (observations)  
      name_s = "qsim", # name of the second time-series data (simulations)  
      name_COSyear = "yyyy", # name of year-column
      name_COSmonth = "mm",  # name of month-column
      name_COSday = "dd",  # name of day-column
      name_COShour = "hh", # name of hour-column
      name_COSmin = "min", # name of minute-column
      name_COSposix = "posixdate", # name of the complete-date-column
      name_COSperiod = "period", # name of the marked-period column
   # plot options
      color_data1 = "steelblue", # color associated with the first time-series data
      color_data2 = "orange",  # color associated with the second time-series data
      legend_title = "legend", 
      xlab = "year",
      color_of_low = 'darkred', # color associated with a bad objective function 
      olor_of_mid = 'grey', # color associated with an average objective function 
      color_of_high = 'magenta', # color associated with a high objective function 
      color_of_out = "grey". # color given to values outside the of range (as defined by of_limits)
      midpoint = 0.5,
      of_limits = c(0,1)
  )
}
}
\examples{
viscos_options("name_o")
viscos_options(name_o = "OtherData")
viscos_options("name_o")
}
\keyword{datasets}
