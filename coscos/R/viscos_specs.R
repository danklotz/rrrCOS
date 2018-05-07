# -------------------------------------------------------------------------
# viscos specs 
# authors: Daniel Klotz, Johannes Wesemann, Mathew Herrnegger 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ------------------------------------------------------------------------

#' Runoff example
#'
#' Returns some examplary runoff raw-data that can be transformed into a \emph{cosdata} \code{tibble}, and used to test the functions of \pkg{visCOS}.
#' @author Daniel Klotz, Johannes Wesemann, Mathew Herrnegger
#' @family viscos specs
#' 
#' @examples 
#' viscos_example() %>% head()
#' 
#' 
#' @export
viscos_example <- function() {
  path <- system.file("extdata", "runoff_example.csv", package = "coscos")
  runoff_example <- read.csv(path)
  return(runoff_example)
}

# --------------------------------------------------------------------------

#' Options for viscos
#'
#' @description 
#' List of options for \pkg{visCOS}. 
#' @author Daniel Klotz, Johannes Wesemann
#'
#' @details 
#' The provided options are listed below´(where standard setting for each option is given alongside a short explanation). Each options can be changed at will. 
#' \itemize{
#'   \item \strong{name_o = "qobs"} ... Name of the \eqn{o}-column (observations).
#'   \item \strong{name_s = "qsim"} ... Name of the \eqn{s}-column (simulations).
#'   \item \strong{name_lb = "lb"} ... Optional lower bounds of the \eqn{s} data. 
#'   \item \strong{name_lb = "ub"} ... Optional upper bounds of the \eqn{s} data.
#'   \item \strong{name_COSyear = "yyyy"} ... Name of the COSERO year-column.
#'   \item \strong{name_COSmonth = "mm"} ... Name of the COSERO month-column.
#'   \item \strong{name_COSday = "dd"} ... Name of the COSERO day-column.
#'   \item \strong{name_COShour = "hh"} ... Name of the COSERO hour-column.
#'   \item \strong{name_COSmin = "min"} ... Name of the COSERO minute-column.
#'   \item \strong{name_COSposix = "posixdate"} ... Name of the complete-date-column.
#'   \item \strong{name_COSperiod = "period"} ... Name of the marked-period-column.
#'   \item \strong{name_COSperiod = "(m^3/s)"} ... Unit-tag.
#'   \item \strong{missing_data = -999} ... Additional marker for missing data.
#'   \item \strong{color_o = "steelblue"} ... Color associated with \eqn{o}-data.
#'   \item \strong{color_o = "orange"} ... Color associated with \eqn{s}-data.
#'   \item \strong{of_limits = c(0,1)} ... Limits for the objective function visualiations
#' }
#'
#' @examples
#' # check "name_o":
#' viscos_options("name_o")
#' 
#' # change and check "name_o":
#' viscos_options(name_o = "other_name")
#' viscos_options("name_o")
#' 
#' @family viscos specs
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
