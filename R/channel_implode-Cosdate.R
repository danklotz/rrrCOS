#' transforms COSdate into the nicer POSIXct-date format
#'
#' Takes a data.frame, which contains the COSdate format (see: xxx) and transforms it into a POSIXct series.
#' Note that time is assumbed to be in UTC
#' @param data_frame A data.frame object containing a time series of the COSdate format
#' @return dates in the format of the POSIXct-class
#' @export
#' @examples 
#' Get runof example and implode cos_date to obtain the POSIXct date
#' require(magrittr)
#' runoff <- pour_runoff_example()
#' runoff %<>% channel_implode_cosdate 
#' # not a fan of magrittr-piping? Just use: runoff <- channel_implode_cosdate(runoff)
#' # 
#' # note that if channel_implode_cosdate is not used on the runoff data istelf it will work like a pour and just return the posix_dates 
#' runoff <- pour_runoff_example()
#' posix_dates <- channel_implode_cosdate(runoff)
#' head(posix_dates)
channel_implode_cosdate <- function(data_frame) {
  # def
  require(magrittr, quietly = TRUE)
  name_string <-  data_frame %>% names %>% tolower
  if (any(name_string == "posixdate")) stop("data_frame does allreay contain POSIXdate")
  # calc
  POSIXdate <- paste(data_frame$yyyy,
                     sprintf("%02d",data_frame$mm),
                     sprintf("%02d",data_frame$dd),
                     sprintf("%02d",data_frame$hh),
                     sprintf("%02d",data_frame$min),
                     sep= "" ) %>%
    as.POSIXct(format = "%Y%m%d%H%M",tz = "UTC")
  return(POSIXdate)
}
