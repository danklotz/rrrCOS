#' Objective Functions
#' 
#' Different objective Functions, provided by visCOS. A detailed description 
#' of each of the provided objective function is provided in the respective
#' vignette
#' 
#' @param o The reference data or observations (o_data)
#' @param s The created data or the simulations (s_data)
#' @name of_overview
NULL
#' Nash-Sutcliffe Efficiency
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_nse <- function(o,s) {
  as.numeric( NSE(s,o) )
}
#' Kling-Gupta Efficiency
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_kge <- function(o,s) {
  as.numeric( KGE(s,o) )
}
#' Percentage Bias 
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_pbias <- function(o,s) {
  as.numeric( pbias(s,o) )
}
#' Correlation
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_cor <- function(o,s) {
  diag( cor(o,s) )
}
#' Root Mean Sqaured Error 
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_rmse <- function(o,s) {
  as.numeric( rmse(s,o) )
}
#' Inverted Nash-Sutcliffe Efficiency
#' 
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_invert_nse <- function(o,s) {
  as.numeric( NSE(o,s) )
}
#' Ratio of Standard Deviations
#'
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_rsd <- function(o,s) {
  as.numeric( rSD(s,o) )
}
#' Ratio of Means
#'
#' @rdname of_overview
#' @export
of_rmeans <- function(o,s) {
  as.numeric( mean(s)/mean(o) )
}
#' Volumetric Efficiency
#'
#' @rdname of_overview
#' @import hydroGOF
#' @export
of_ve <- function(o,s) {
  as.numeric( VE(s,o) )
}
