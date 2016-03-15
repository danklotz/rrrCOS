#' Convert runoff_data to xts-format
#'
#' Converts the runoff_data (class: data_frame) into an xts object
#'
#' @param runoff_data data_frame of the runoff_data (see: xxx)
#' @return xts object of the runoff_data data.frame
#' @export
pour_runoff_as_xts <- function(runoff_data) {
  # pre
  require(xts, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  assert_dataframe(runoff_data)
  assert_chunk(runoff_data)
  # calculations:
  runoff_data_as_xts <- xts::xts(x = runoff_data, order.by = runoff_data$POSIXdate) %>% channel_names
  # 
  return(runoff_data_as_xts)
}
