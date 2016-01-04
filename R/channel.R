#' channels the filename away
#' 
#' Removes the filename from a path 
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

#' channels the basins withouth observation away
#' 
#' Removes basins withouth observation (-999/NA values) away from the provided dataframe 
#' 
#' @param d_AllBasins A raw runoff dataframe, which may containts basins withouth observations. 
#' \strong{Note:} It is assumed that all available basins are simulated!
#' @return data.frame without the observation-free basins
#' @export
channel.NoSim <- function(d_AllBasins) {
  if ( is.data.frame(d_AllBasins) ) {
    d_AllBasins[is.na(d_AllBasins)] = -999
    colmax <- function(x) lapply(X = d_AllBasins, FUN = max) # get max values of each column
    idx_temp <- which(colmax(d_AllBasins) == -999)
    idx_slct <- sort(c(idx_temp,idx_temp+1,idx_temp+2))
    d_onlyObserved <- d_AllBasins[-idx_slct]
    return(d_onlyObserved)
  } else {
    stop("Input Data must be a data.frame in the right format")
  }
}


#' transforms COSdate into a nice date format
#' 
#' Takes a data_frame containing the COSdate format and transforms it into a POSIX date series
#' @param data_frame A data.frame object containing a time series of the COSdate format
#' @return dates in the format of the POSIXct-class 
#' @export
implode.Cosdate <- function(data_frame) {
  POSIXdate <- paste(data_frame$yyyy,
                     sprintf("%02d",data_frame$mm),
                     sprintf("%02d",data_frame$dd),
                     sprintf("%02d",data_frame$hh),
                     sprintf("%02d",data_frame$min),
                     sep= "" ) %>% 
                 as.POSIXct(format = "%Y%m%d%H%M",tz="UTC")
  return(POSIXdate)
}

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
  # defense 
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
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



#' Explore runoff_data time series with shiny
#' 
#' Runs a Shiny App which can be used to get an overview of a runoff_data time series object
#' 
#' @param d_xts runoff_data formatted as time series
#' @export
explore.runoff <- function(d_xts) {
  ##########################
  # pre
  ##########################
  require("data.table") 
  require("magrittr")
  require("shiny")
  ##########################
  # defences
  ##########################
  
  ##########################
  # calc
  ##########################
  # get d_nums 
  names <- names(d_xts)
  names %<>% gsub('.*_', '' ,.) %>% unique(.) # replaces everything before "_" and gets the unique names
  d_nums <- names[!grepl("[a-z]",names)] %>% as.integer(.)
  
  runApp("R/AppExplore")
}
