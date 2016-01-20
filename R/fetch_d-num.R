#' Get numbers of the different basins 
#'
#' @param runoff_data runoff data_frame (see: xxx)
#' @return vector of integers containing the numbers of the given basins
#' @export
fetch.d_num <- function(runoff_data) {
  require(magrittr)
  assert.dataframe(runoff_data)
  assert.Chunk(runoff_data)
  #
  d_names <- names(runoff_data)
  d_nums <- d_names  %>% gsub('\\D','',.) %>% unique
  d_nums <- d_nums[!(d_nums == "")] %>% as.integer
  return(d_nums)
}
