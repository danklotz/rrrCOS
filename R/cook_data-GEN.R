  #' Get runoff example
  #' 
  #' Get exemplary runoff data to test the different functions of visCOS
  #' @export
  get_runoff_example <- function() {
    file_path <- system.file("extdata", 
                             "runoff_example.csv", 
                             package = "visCOS")
    runoff_example <- read.csv(file_path)
    return(runoff_example)
  }
  #' removes chunk in runoff_data
  #' 
  #' Remove all collumns which are not foreseen (see: viscos_options) from 
  #' runoff data 
  #' 
  #' @param runoff_data data.frame object containing at lesast COSdate, 
  #' Qsim and Qobs (see: xxx)
  #' @return data.frame object withouth the chunk
  #' @export
  remove_chunk <- function(runoff_data) {
  require("magrittr", quietly = TRUE)
  # 
  if ( !is.data.frame(runoff_data) ){
    stop("Input Data must be a data.frame object")
  } 
  lowercase_names_in_data <- runoff_data %>% names %>% tolower 
  # 
  regex_columns <- paste("^",viscos_options()$name_COSyear,"$|", 
                         "^",viscos_options()$name_COSmonth,"$|",
                         "^",viscos_options()$name_COSday,"$|",
                         "^",viscos_options()$name_COShour,"$|",
                         "^",viscos_options()$name_COSmin,"$|",
                         viscos_options()$name_COSobs,".*|",
                         viscos_options()$name_COSsim,".*|",
                         viscos_options()$name_COSposix,"|",
                         viscos_options()$name_COSperiod,
                         sep = "")

  idx <- regex_columns %>% 
    grep(.,lowercase_names_in_data)
    no_chunk_runoff_data <- runoff_data[ , idx]
    return( only_observed_basins(no_chunk_runoff_data) )
  }
# remove basins withouth observations
#
# Removes basins withouth observation (-999/NA values) from the provided dataframe
#
# @param runoff_data A raw runoff_data data.frame, which may containts basins 
# withouth observations.
# \strong{Note:} It is assumed that all available basins are simulated!
# @return data.frame without the observation-free basins
# @export
only_observed_basins <- function(runoff_data) {
  require("magrittr", quietly = TRUE)
  assert_dataframe(runoff_data)
  runoff_data[is.na(runoff_data)] <- -999 
  colmax <- lapply(X = runoff_data, FUN = max) # get max of any column
  if ( any(colmax < 0.0) ){
    idx_temp <- which(colmax < 0.0) 
    obs_regex <- paste(viscos_options()$name_COSobs,".*", sep ="")
    OnlyQobsSelected <- idx_temp %>%
      names %>% 
      tolower %>%
      grepl(obs_regex,.) %>%
      any
    if (!OnlyQobsSelected){
      stop("There are Qsim withouth simulation (i.e. only values smaller 0). Pls remove them first")
    }
    idx_slct <- c(idx_temp,idx_temp + 1) %>% sort() 
    d_onlyObserved <- runoff_data[-idx_slct]
    # set remaining negative Qobs to NA, so that HydroGOF can be used correctly, also ignoring NAs
    colmin <- lapply(X = d_onlyObserved, FUN = min)
    idx_temp <- which(colmin < 0.0) 
    d_onlyObserved[d_onlyObserved[idx_temp]<0, idx_temp] <- NA
    return(d_onlyObserved)
  } else {
    return(runoff_data)
  }
}


#' Complete the date-formats with POSIXct or COSdate
#' 
#' Complete the data-formats of your data.frame `POSIXct` and/or `COSdate`
#' 
#' @param runoff_data The data.frame, which contains the runoff information
#' @param name_cosyear string with the name of the `COSdate` year column
#' @param name_posi string with the name of the POSIXct column 
#' @return The new runoff data.frame with the added data-format. 
#' @export
prepare.complete_date <- function(runoff_data = NULL, 
                                  name_cosyear = "yyyy",
                                  name_posix = "POSIXdate") {
  # 1. make sure that magrittr is loaded: 
  require("magrittr", quietly = TRUE)
  # 2. check if the runoff_data is a data.frame
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  # 3. Check for COSdates and stop if non-logical expression are obtained
  OK_COSdate <- any(names(name_cosyear)=="yyyy")
  OK_POSIXdates <- any(names(name_posix)=="POSIXdate")
  if ( !is.logical(OK_COSdate) | !is.logical(OK_POSIXdates) ) {
    stop("Something seems to be wrong with the date / time formats :(")
  }
  # choose function depending on which formats are available!
  if (!OK_COSdate & !OK_POSIXdates) {
    stop("No COSdates and no POSIXct-dates in the data!")
  } else if (OK_COSdate & !OK_POSIXdates) { 
    runoff_data <- implode_cosdate(runoff_data) # see following chapter
  } else if (!OK_COSdate & OK_POSIXdates) {
    stop("POSIXct to COSdates not yet supported :(")
  }
  return(runoff_data)
}
# transform COSdate into the nicer POSIXct-date format
#
# Takes a data.frame, which contains the COSdate format (see: xxx) and 
# transforms it into a POSIXct series. Note that time is assumed to be in UTC
implode_cosdate <- function(runoff_data) {
  require("magrittr", quietly = TRUE)
  # 
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  #
  name_string <-  runoff_data %>% names %>% tolower
  if (any(name_string == viscos_options()$name_COSposix)) stop("runoff_data does allreay contain POSIXdate")
  POSIXdate <- paste(runoff_data[viscos_options()$name_COSyear],
                     sprintf("%02d",runoff_data[viscos_options()$name_COSmonth]),
                     sprintf("%02d",runoff_data[viscos_options()$name_COSday]),
                     sprintf("%02d",runoff_data[viscos_options()$name_COShour]),
                     sprintf("%02d",runoff_data[viscos_options()$name_COSmin]),
                     sep= "" ) %>%
    as.POSIXct(format = "%Y%m%d%H%M",tz = "UTC")
  return(cbind(runoff_data,POSIXdate))
}

remove_leading_zeros <- function(runoff_data) {
  require("magrittr", quietly = TRUE)
  runoff_data <- remove_chunk(runoff_data)
  runoff_names <- runoff_data %>% names %>% gsub("\\d","",.) 
  # get numbers and remove leading zeros
  runoff_nums <- runoff_data %>% 
    names %>% 
    gsub("\\D","",.) %>% 
    as.numeric %>%  
    as.character
  runoff_nums[is.na(runoff_nums)] = ""
  # paste new nums as new data_names 
  names(runoff_data) <- paste(runoff_names, runoff_nums, sep = "")
  return(runoff_data)
}
