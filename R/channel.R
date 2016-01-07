###################################################################################################
# channel fucntions are used to manipulate data cut stuff away etc. 

###################################################################################################

###################################################################################################
#' Removes the filename from a path 
#' 
#' Removes the filename from a given path 
#' @param filepath path to a given file
#' @return path to the file
#' @export
channel.path <- function(filepath) {
  ##########################
  # pre
  ##########################
  require(magrittr)
  ##########################
  # calc
  ##########################
  if (exists("filepath")) {
    # check for system depented path 
    sysIswindos <- grepl('\\\\',filepath)
    if (sysIswindos) {
      channel.path <- filepath %>%
        strsplit("\\\\") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(channel.path)
    } else { 
      channel.path <- filepath %>%
        strsplit("/") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(channel.path)
    }
  } else { 
    stop("no filepath provided!")
#       channel.path  <- file.choose() %>%
#         strsplit("\\\\") %>% .[[1]] %>% 
#         strsplit("/") %>% .[[1]] %>% 
#         .[1:(length(.)-1)] %>% 
#         paste(.,collapse = "/")
#       return(channel.path)
    }
}


###################################################################################################
#' remove basins withouth observations
#' 
#' Removes basins withouth observation (-999/NA values) away from the provided dataframe 
#' 
#' @param runoff_data A raw runoff_data data.frame, which may containts basins withouth observations. 
#' \strong{Note:} It is assumed that all available basins are simulated!
#' @return data.frame without the observation-free basins
#' @export
channel.onlyObserved <- function(runoff_data) {
  # defences
    if ( !is.data.frame(runoff_data) ){
      stop("Input Data must be a data.frame in the runoff_data format (see: help xxx)")
    } 
  # 
  runoff_data[is.na(runoff_data)] = -999
  colmax <- function(x) lapply(X = runoff_data, FUN = max) # get max values of each column
  idx_temp <- which(colmax(runoff_data) == -999)
  idx_slct <- sort(c(idx_temp,idx_temp+1,idx_temp+2))
  d_onlyObserved <- runoff_data[-idx_slct]
  return(d_onlyObserved)

}

###################################################################################################
#' removes chunk in runoff_data
#' 
#' Removes all collumns which are not foreseen in the runoff_data format (see: xxx)
#' 
#' @param runoff_data data.frame object containing at lesast COSdate, Qsim and Qobs (see: xxx)
#' @return data.frame object withouth the chunk
#' @export
channel.removeChunk <- function(runoff_data) {
  # pre
    require(dplyr)
  # defences
    if ( !is.data.frame(runoff_data) ){
      stop("Input Data must be a data.frame object")
    } 
  # calc
  runoff_data <- select(runoff_data, 
                matches("yyyy"),
                matches("mm"),
                matches("dd"),
                matches("hh"),
                matches("min"),
                matches("Qsim|Qobs"), 
                matches("POSIXdate|hydyear"))
    return(runoff_data)
}


###################################################################################################
#' transforms COSdate into the nicer POSIXct-date format
#' 
#' Takes a data.frame, which contains the COSdate format (see: xxxx) and transforms it into a POSIXct series.
#' Note that time is assumbed to be in UTC
#' @param data_frame A data.frame object containing a time series of the COSdate format
#' @return dates in the format of the POSIXct-class 
#' @export
implode.Cosdate <- function(data_frame) {
  if (any(names(data_frame == "POSIXdate"))) stop("data_frame dos allreay contain POSIXdate") 
  #
  POSIXdate <- paste(data_frame$yyyy,
                     sprintf("%02d",data_frame$mm),
                     sprintf("%02d",data_frame$dd),
                     sprintf("%02d",data_frame$hh),
                     sprintf("%02d",data_frame$min),
                     sep= "" ) %>% 
                 as.POSIXct(format = "%Y%m%d%H%M",tz = "UTC")
  return(POSIXdate)
}


###################################################################################################
#' calculate hydrological years
#' 
#' @param runoff_data The data.frame, which contains the runoff information
#' @return The runoff data.frame reduced and ordered according to the hydrological years within the data. 
#' \strong{Note:} The hydrological years are formatted as characters.
#' @export
channel.hydyears <- function(runoff_data) {
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  #
  if ( !exists("POSIXdate", where = runoff_data) & !exists("yyyy", where = runoff_data) ) {
    stop("data.frame does neiter contain POSIXdate nor COSdate")
  } else if ( exists("POSIXdate", where = runoff_data) & !exists("yyyy", where = runoff_data) ) {
    stop("transformation from POSIXdate to COSdate not yet available :(")
  } else if ( !exists("POSIXdate", where = runoff_data) & exists("yyyy", where = runoff_data) ) {
    runoff_data$POSIXdate <- implode.Cosdate(runoff_data)
  }
  #
  years <- fetch.yearsindata(runoff_data)
  num_years = length(years$in_data)
  runoff_data$hydyear <- as.character(runoff_data$POSIXdate)
  hydyears_in_d <- fetch.hydyears(runoff_data,years)
  num_hydyears <- length(hydyears_in_d)
  # calculate and format hydrological years
  cnt <- 0
  for (i in 1:(num_hydyears)) 
  {
    tmp_d_YearX <- filter(runoff_data, yyyy == years$in_data[i] | yyyy == years$in_data[i+1])  %>% 
      filter(.,(yyyy == years$in_data[i] & mm >= 9 ) | (yyyy == years$in_data[i+1] & mm < 9 ) ) %>%
      select(hydyear) %>%
      transform(hydyear = hydyears_in_d[i])
    tmp_lngth <- dim(tmp_d_YearX)[1]
    runoff_data$hydyear[(cnt+1):(cnt+tmp_lngth)] <- tmp_d_YearX$hydyear
    cnt = cnt + tmp_lngth
  }
  return(runoff_data)
}


###################################################################################################
#' complete the date-formats with xts or COSdate
#' 
#' Completes the data formats of the runoff_data data.frame by adding either the needed xts-dates or COSdates
#' 
#' @param runoff_data The data.frame, which contains the runoff information
#' @return The new runoff data.frame with the added data-format. 
#' @export
channel.completeDate <- function(runoff_data) {
  # pre 
  require(magrittr)
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  # 
  # calc
  OK_Cosdates <- any(names(runoff_data)=="yyyy")
  OK_POSIXdates <- any(names(runoff_data)=="POSIXdate")
  if ( is.logical(OK_Cosdates) & is.logical(OK_POSIXdates) ) {
    if (!OK_Cosdates & !OK_POSIXdates) {
      stop("No COSdates and no POSIXct-dates in the data!")
    } else if (OK_Cosdates & !OK_POSIXdates) { 
      runoff_data$POSIXdate <- implode.Cosdate(runoff_data)
    } else if (!OK_Cosdates & OK_POSIXdates) {
      stop("POSIXct to COSdates not yet supported :(")
    }
  } else { 
    stop("Something seems to be wrong with the date and time formats :(")
  }
  return(runoff_data)
}


###################################################################################################
#' Convert runoff_data to xts-format 
#' 
#' Converts the runoff_data (class: data_frame) into an xts object
#' 
#' @param runoff_data data_frame of the runoff_data (see: xxx)
#' @return xts object of the runoff_data data.frame
#' @export
channel.dxts <- function(runoff_data) {
  # pre
    require(dplyr)
    testfor.dataframe(runoff_data)
    testfor.Chunk(runoff_data)
  # calculations:
  d_xts <- d_runoff %>% 
    filter(yyyy >= ctrl$ctrl_span[1],yyyy <= ctrl$ctrl_span[2])
  return(d_xts)
}










