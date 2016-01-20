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