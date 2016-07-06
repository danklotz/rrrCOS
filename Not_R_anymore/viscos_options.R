#' visCOS options
#' 
#' Get and modify visCOS global options
#' This function provides such a list with the basic settings added. 
#' @export
viscos_options <- GlobalOptions::setGlobalOptions( 
  name_COSobs = "qobs", 
  name_COSsim = "qsim", 
  name_COSyear = "yyyy",
  name_COSmonth = "mm",
  name_COSday = "dd",
  name_COShour = "hh",
  name_COSmin = "min",
  name_COSposix = "posixdate",
  name_COSperiod = "period",
  # plot stuff
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
  text_size = 0.5
)
