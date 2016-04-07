# test for chunkin data
#
# Tests if there is still chunk in the runoff_data data frame. 
# Where chunk is every data frame except does definded by `get_regex_for_runoff_data`
# example: 
# # get some example data:
# runoff_data <- pour(runoff_example) 
# assert_chunk(runoff_data)
# # remove chunk from runoff data: 
# no_chunk_data <- prepare.remove_chunk(runoff_data)
# assert_chunk(no_chunk_data)
assert_chunk <- function(runoff_data) {
  require(magrittr, quietly = TRUE)
  regEx <- get_regex_for_runoff_data()
  assertChunk <- names(runoff_data) %>% tolower %>% grepl(regEx,.)
  if (any(assertChunk == FALSE)) stop("there is still chunk in the data, try: prepare.removeChunk")
}
