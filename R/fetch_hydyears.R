#' Get hydrological years of the runoff_data
#' 
#' @param runoff_data runoff data.frame as defined in xxx
#' @param years_in_data list as returned by \code{\link[visCOS]{fetch.years_in_data}}
#' @return list with (short named) hydrological years
#' @export
fetch.hydyears <- function(runoff_data,years_in_data) {
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  if ( missing(years) ) {
    years <- fetch.yearsindata(runoff_data)
  }
  #
  #§ bad code :(
  lngth_sim <- dim(runoff_data)[1]
  start <- 1
  lefin <- length(years$in_data_shrt) - 1 # at least 1 year less then normal years
  if (runoff_data$mm[1] > 9) start <- start + 1
  if (runoff_data$mm[lngth_sim] < 9) lefin <- lefin - 1
  hydyears_in_d <- years$in_data_shrt[start:lefin]
  num_hydyears <- length(hydyears_in_d)
  for (i in 1:(num_hydyears)) {
    hydyears_in_d[i] <- paste(years$in_data_shrt[i],years$in_data_shrt[i+1], sep = "/")
  }
  return(hydyears_in_d)
  #§
}