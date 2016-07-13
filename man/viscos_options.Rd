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
      name_data1 = "qobs",
      name_data2 = "qsim"
      name_COSyear = "yyyy",
      name_COSmonth = "mm",
      name_COSday = "dd",
      name_COShour = "hh",
      name_COSmin = "min",
      name_COSposix = "posixdate",
      name_COSperiod = "period",
   # plot options
      color_data1 = "steelblue", 
      color_data2 = "palevioletred",
      plot_title = "title",
      legend_title = "legend",
      xlab = "year",
      clr1 = '#FF3300',
      clr2 = '#f6f3a1',
      clr3 = '#005900',
      clr4 = "purple4",
      midpoint = 0.5,
      limits = c(0,1),
      lb_cut = 0.0,
      text_size = 0.5)}
}
\examples{
viscos_options("name_data1")
viscos_options(name_data1 = "OtherData")
viscos_options("name_data1")
}
\keyword{datasets}
