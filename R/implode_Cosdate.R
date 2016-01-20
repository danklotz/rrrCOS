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