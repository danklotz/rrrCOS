#' Extract spinup time from stats file
#' 
#' Spinup-time is an integer. It is the time in hours, which is not used for the model-evalution. 
#' The pattern is the used marker within the file
#' 
#' 
#' @param filepath path to the given file
#' @param pattern string used to mark that the next integer is the spinup time
#' @return Integer indicating the length of the spin-up time in hours
#' @importFrom magrittr %>%
#' @export
fetch.spinup <- function(filepath,pattern) {
  tmp <- filepath %>% paste %>% readLines
  spinup <- grep(pattern,tmp) %>%
    tmp[.] %>% 
    sub('.*:', '',.) %>% 
    as.integer(.) + 1
  return(spinup)
}

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

#' Get hydrological years of the runoff_data
#' 
#' @return list with (short named) hydrological years
#' @export
fetch.hydyears <- function(runoff_data,years) {
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  if ( missing(years) ) {
    years <- fetch.yearsindata(runoff_data)
  }
  #
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
}

#' Get years of the runoff_data
#' 
#' @return list with (short and long named) years. \strong{Note:} Short names years are just characters.
#' Thus, they may only be usefull for plotting
#' @export
fetch.yearsindata <- function(runoff_data) {
  require(magrittr)
  #
  # defense 
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  # calc
  years <- list()
  years$in_data <- unique(runoff_data$yyyy)
  years$in_data_shrt <- years$in_data %>% 
                          unique %>% 
                          as.character %>% 
                          substring(.,3,4)
  return(years)
}

#' Get some objective functions (OF)
#' 
#' Get some basic objective functions used in hydrology, i.e.: Root Mean Squared Error, Correlation, NSE, KGE, pbias
#' @return data.frame contianing basic OF
#' @export
fetch.hydOF <- function(obs,sim) {
  require(hydroGOF)
  require(magrittr)
  # calc
  out <- data.frame(
    RMSE = -999,
    corr = -999, 
    NSE = -999, 
    KGE = -999, 
    pbias = -999
  )
  out$RMSE <- rmse(sim,obs) %>% as.numeric
  out$corr <- cor(sim,obs) %>% diag(.)
  out$NSE <- NSE(sim,obs) %>% as.numeric
  out$KGE <- KGE(sim,obs) %>% as.numeric
  out$pbias <- pbias(sim,obs)
  return(out)
}

