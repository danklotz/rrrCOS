#' complete the date-formats with xts or COSdate
#' 
#' 
#' 
#' @param runoff_data The data.frame, which contains the runoff information
#' @return The new runoff data.frame with the added data-format. 
#' @export
channel_complete_date <- function(runoff_data) {
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