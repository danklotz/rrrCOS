#' transforms COSdate into the nicer POSIXct-date format
#' 
#' Takes a data.frame, which contains the COSdate format (see: xxxx) and transforms it into a POSIXct series.
#' Note that time is assumbed to be in UTC
#' @param data_frame A data.frame object containing a time series of the COSdate format
#' @return dates in the format of the POSIXct-class 
#' @export
implode.Cosdate <- function(data_frame) {
  # def
  name_string <-  data_frame %>% names %>% tolower 
  if (any(name_string == "posixdate")) stop("data_frame dos allreay contain POSIXdate") 
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