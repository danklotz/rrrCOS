#' visCOS global options
#' 
#' Get and set the global options of visCOS
#' 
#' These are the options you can adapt by executing the function 
#' (default values)
#' \preformatted{
#'   viscos_options(
#'    # data.frame column names
#'       name_data1 = "qobs",
#'       name_data2 = "qsim",
#'       name_COSyear = "yyyy",
#'       name_COSmonth = "mm",
#'       name_COSday = "dd",
#'       name_COShour = "hh",
#'       name_COSmin = "min",
#'       name_COSposix = "posixdate",
#'       name_COSperiod = "period",
#'    # plot options
#'       color_data1 = "steelblue", 
#'       color_data2 = "palevioletred",
#'       plot_title = "title",
#'       legend_title = "legend",
#'       xlab = "year",
#'       color_of_low = '#FF3300',
#'       olor_of_mid = '#f6f3a1',
#'       color_of_high = '#005900',
#'       clr4 = "purple4",
#'       midpoint = 0.5,
#'       limits = c(0,1),
#'       lb_cut = 0,
#'       text_size = 0.5)}
#'
#' @examples
#' viscos_options("name_data1")
#' viscos_options(name_data1 = "OtherData")
#' viscos_options("name_data1")
#' @export

viscos_options <- GlobalOptions::setGlobalOptions( 
  # data.frame column names
  name_data1 = "qobs", 
  name_data2 = "qsim", 
  name_COSyear = "yyyy",
  name_COSmonth = "mm",
  name_COSday = "dd",
  name_COShour = "hh",
  name_COSmin = "min",
  name_COSposix = "posixdate",
  name_COSperiod = "period",
 # plot options
  color_data1 = "steelblue", 
  color_data2 = "orange",
  plot_title = "title",
  legend_title = "legend",
  xlab = "year",
  color_of_low = 'purple4',
  color_of_mid = 'orange',
  color_of_high = 'green',
  color_of_out = "red",
  midpoint = 0.5,
  limits = c(0,1),
  lb_cut = 0,
  text_size = 0.5
)

