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
