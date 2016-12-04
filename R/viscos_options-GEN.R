#' visCOS global options
#' 
#' Get and set the global options of visCOS
#' 
#' These are the options you can adapt by executing the function 
#' (default values)
#' \preformatted{
#'   viscos_options(
#'    # data.frame column names
#'       name_o = "qobs", # name of the first time-series data (observations)  
#'       name_s = "qsim", # name of the second time-series data (simulations)  
#'       name_COSyear = "yyyy", # name of year-column
#'       name_COSmonth = "mm",  # name of month-column
#'       name_COSday = "dd",  # name of day-column
#'       name_COShour = "hh", # name of hour-column
#'       name_COSmin = "min", # name of minute-column
#'       name_COSposix = "posixdate", # name of the complete-date-column
#'       name_COSperiod = "period", # name of the marked-period column
#'    # plot options
#'       color_o = "steelblue", # color associated with the first o time-series data
#'       color_s= "orange",  # color associated with the second s time-series data
#'       of_limits = c(0,1) # limits of the plotted objective functions
#'   )
#' }
#'
#' @examples
#' viscos_options("name_o")
#' viscos_options(name_o = "OtherData")
#' viscos_options("name_o")
#' @export
viscos_options <- GlobalOptions::setGlobalOptions( 
  # data.frame column names
  name_o = "qobs", 
  name_s = "qsim", 
  name_COSyear = "yyyy",
  name_COSmonth = "mm",
  name_COSday = "dd",
  name_COShour = "hh",
  name_COSmin = "min",
  name_COSposix = "posixdate",
  name_COSperiod = "period",
 # plot options
  color_o = "steelblue", 
  color_s = "orange",
  of_limits = c(0,1)
)

