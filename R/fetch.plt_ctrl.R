#' get basic config for the plot control
#' 
#' xxx description has to follow 
#' @export
fetch_plt_ctrl <- function(ctrl) {
  # def
  if ( missing(ctrl) ) {ctrl <- fetch_ctrl()}
  # calc
  plt_ctrl <- list() # reset list
  plt_ctrl$gtitle <- "Title"
  plt_ctrl$ltitle <- "Legend"
  plt_ctrl$xlab <- ctrl$yearName
  plt_ctrl$clr1 <- ctrl$colors[1]
  plt_ctrl$clr2 <- ctrl$colors[2]
  plt_ctrl$clr3 <- ctrl$colors[3]
  plt_ctrl$clr4 <- ctrl$colors[4]
  plt_ctrl$midpoint <- ctrl$clr_NSEmid
  plt_ctrl$limits <- c(0,1)
  plt_ctrl$lb_cut <- 0.0
  plt_ctrl$OFsize <- ctrl$OFsize
  return(plt_ctrl)
}