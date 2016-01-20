#' Sets ctrl with arbitrary values
#' 
#' Some pre-sets for the ctrl list
#' 
#' @return list with arbitrary settings of ctrl options
#' @export
fetch.ctrl <- function() {
  ctrl <- list()
  # ******************
  # arbitrary presets:
  ctrl$ofoldername <- "test"
  # Interactive Overview: 
  ctrl$ctrl_span  	<- c(2009,2012) 
  # OF plot options:
  # naming:
  ctrl$yearName   	<- "Jahr" 
  # color-settings:
  ctrl$colors      		<- c('#FF3300','#f6f3a1','#005900',"purple4") 
  ctrl$clr_NSEmid  	<- 0.5 
  ctrl$OFsize       <- 5.5
  #
  return(ctrl)
}