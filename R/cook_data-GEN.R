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
    return(runoff_data)
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
