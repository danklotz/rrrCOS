# Get numbers of the different basins
pour.number_of_basins <- function(runoff_data) {
  require(magrittr)
  assert_dataframe(runoff_data)
  assert_chunk(runoff_data)
  #
  d_names <- names(runoff_data)
  d_nums <- d_names  %>% gsub('\\D','',.) %>% unique
  d_nums <- d_nums[!(d_nums == "")] %>% as.integer
  return(d_nums)
}
