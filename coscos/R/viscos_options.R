# --------------------------------------------------------------------------
# viscos specifica 
# authors: Daniel Klotz, Johannes Wesemann, Mathew Herrnegger
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# --------------------------------------------------------------------------

#' Runoff Example
#'
#' Returns some examplary runoff data (i.e. raw \code{cosdata}) for 
#' \pkg{viscos}
#' 
#' @family viscos specifica
#' 
#' @export
viscos_example <- function() {
  path <- system.file("extdata", "runoff_example.csv", package = "visCOS")
  runoff_example <- read.csv(path)
  return(runoff_example)
}

# --------------------------------------------------------------------------

#' Options for viscos
#'
#' Get an optionlist for \pkg{viscos}
#'
#' These are the options you can adapt by executing the function
#' (default values)
#' \preformatted{
#'   viscos_options(
#'    # data.frame column names
#'       name_o = "qobs", # name of the first time-series data, i.e. the observations  
#'       name_s = "qsim", # name of the second time-series data, i.e. the simulations  
#'       name_lb = "lb", # lower bound information of the simulations 
#'       name_ub = "ub", # upper bound information of the simulations 
#'       name_COSyear = "yyyy", # name of year-column
#'       name_COSmonth = "mm",  # name of month-column
#'       name_COSday = "dd",  # name of day-column
#'       name_COShour = "hh", # name of hour-column
#'       name_COSmin = "min", # name of minute-column
#'       name_COSposix = "posixdate", # name of the complete-date-column
#'       name_COSperiod = "period", # name of the marked-period column
#'      data_unit = "(m^3/s)", # unit-tag o the simulation and observation data
#'       missing_data = -999, # marker for missing data in the o_columns
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
#' 
#' @family viscos specifica
#' 
#' @export
viscos_options <- GlobalOptions::setGlobalOptions(
  # data.frame column names
  name_o = "qobs",
  name_s = "qsim",
  name_lb = "lb",
  name_ub = "ub",
  name_COSyear = "yyyy",
  name_COSmonth = "mm",
  name_COSday = "dd",
  name_COShour = "hh",
  name_COSmin = "min",
  name_COSposix = "posixdate",
  name_COSperiod = "period",
  data_unit = "(m^3/s)",
  missing_data = -999,
 # plot options
  color_o = "dodgerblue",
  color_s = "orange",
  of_limits = c(0,1)
)
