# transforms COSdate into the nicer POSIXct-date format
#
# Takes a data.frame, which contains the COSdate format (see: xxx) and transforms it into a POSIXct series.
# Note that time is assumbed to be in UTC
# @param runoff_data A data.frame object containing a time series of the COSdate format
# @return dates in the format of the POSIXct-class
# @export
# @examples 
# Get runof example and implode cos_date to obtain the POSIXct date
# require(magrittr)
# runoff <- pour_runoff_example()
# runoff %<>% implode_cosdate 
# # not a fan of magrittr-piping? Just use: 
# runoff <- implode_cosdate(pour_runoff_example())
implode_cosdate <- function(runoff_data) {
  # def
    require(magrittr, quietly = TRUE)
    name_string <-  runoff_data %>% names %>% tolower
    if (any(name_string == "posixdate")) stop("runoff_data does allreay contain POSIXdate")
  # calc
  POSIXdate <- paste(runoff_data$yyyy,
                     sprintf("%02d",runoff_data$mm),
                     sprintf("%02d",runoff_data$dd),
                     sprintf("%02d",runoff_data$hh),
                     sprintf("%02d",runoff_data$min),
                     sep= "" ) %>%
    as.POSIXct(format = "%Y%m%d%H%M",tz = "UTC")
  return(cbind(runoff_data,POSIXdate))
}
