#' Get hydrological years of the runoff_data
#' 
#' @param runoff_data runoff data.frame as defined in xxx
#' @param years_in_data list as returned by \code{\link[visCOS]{pour_years_in_data}}
#' @return list with (short named) hydrological years
#' @export
pour_hydyears <- function(runoff_data,years_in_data) {
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  if ( missing(years_in_data) ) {
    years_in_data <- pour_years_in_data(runoff_data)
  }
  #
  #§ bad code :(
  lngth_sim <- dim(runoff_data)[1]
  start <- 1
  lefin <- length(years_in_data$in_data_shrt) - 1 # at least 1 year less then normal years
  if (runoff_data$mm[1] > 9) start <- start + 1
  #§ Problem, if time series ends at end of hydrological years; rough & dirty solution, only valid for hourly data
  if (runoff_data$mm[lngth_sim] < 9 & (runoff_data$mm[lngth_sim] == 8 & runoff_data$hh[lngth_sim] != 23 )) lefin <- lefin - 1
  hydyears_in_d <- years_in_data$in_data_shrt[start:lefin]
  num_hydyears <- length(hydyears_in_d)
  for (i in 1:(num_hydyears)) {
    hydyears_in_d[i] <- paste(years_in_data$in_data_shrt[i],years_in_data$in_data_shrt[i+1], sep = "/")
  }
  return(hydyears_in_d)
  #§
}