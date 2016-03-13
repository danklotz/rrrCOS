#' Sets ctrl with arbitrary values
#' 
#' Retruns the ctrl list with some abitrary pre-sets, which can be changed later on. 
#' 
#' @return list with arbitrary settings of ctrl options
#' @export
fetch_ctrl <- function( of_foldename = "test",
                        ctrl_span = c(2009,2012),
                        year_name = "year", 
                        colors = c('#FF3300',
                                   '#f6f3a1',
                                   '#005900',
                                   "purple4"), 
                        nse_midpoint = 0.5) {
  ctrl <- list()
  # ******************
  # arbitrary presets:
  ctrl$of_oldername <- of_foldename
  # Interactive Overview: 
  ctrl$ctrl_span  	<- ctrl_span
  # OF plot options:
  # naming:
  ctrl$year_name <- "year"
  # color-settings:
  ctrl$colors <- colors
  ctrl$nse_midpoint <- nse_midpoint 
  #
  return(ctrl)
}