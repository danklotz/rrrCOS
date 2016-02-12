#' Convert runoff_data to xts-format
#'
#' Converts the runoff_data (class: data_frame) into an xts object
#'
#' @param runoff_data data_frame of the runoff_data (see: xxx)
#' @return xts object of the runoff_data data.frame
#' @export
channel.runoff_as_xts <- function(runoff_data) {
  # pre
  require(xts, quietly = TRUE)
  assert.dataframe(runoff_data)
  assert.Chunk(runoff_data)
  # calculations:
  runoff_data_as_xts <- xts::xts(x = runoff_data, order.by = runoff_data$POSIXdate)
  return(runoff_data_as_xts)
}
