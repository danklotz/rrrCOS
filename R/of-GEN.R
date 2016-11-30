#' Objective Functions
#' 
#' Different objective Functions, provided by visCOS.

#' @param x observation 
#' @param y simulations 
#' @name of_overview
NULL
#' Root Mean Sqaured Error 
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_rmse <- function(x,y) {
  as.numeric( rmse(y,x) )
}
#' Percentage Bias 
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_pbias <- function(x,y) {
  as.numeric( pbias(y,x) )
}
#' Nash-Sutcliffe Efficiency
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_nse <- function(x,y) {
  as.numeric( NSE(y,x) )
}
#' Inverted Nash-Sutcliffe Efficiency
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_invert_nse <- function(x,y) {
  as.numeric( NSE(x,y) )
}
#' Kling-Gupta Efficiency
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_kge <- function(x,y) {
  as.numeric( KGE(y,x) )
}
#' Correlation
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_cor <- function(x,y) {
  diag( cor(x,y) )
}
#' Ratio of Standard Deviations
#'
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_rsd <- function(x,y) {
  as.numeric( rSD(y,x) )
}
#' Ratio of Means
#'
#' @rdname of_overview
#' @export
of_rmeans <- function(x,y) {
  as.numeric( mean(y)/mean(x) )
}
#' Volumetric Efficiency
#'
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_ve <- function(x,y) {
  as.numeric( VE(y,x) )
}
