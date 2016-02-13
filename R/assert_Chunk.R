# test for chunkin data
#
# tests if there is still chunk in the runoff_data data frame. 
# Where chunk is every data frame except does definded by `fetch.regex_for_runoff_data`
assert.chunk <- function(runoff_data) {
  require(magrittr)
  regEx <- fetch.regex_for_runoff_data()
  assertChunk <- names(runoff_data) %>% tolower %>% grepl(regEx,.)
  if (any(assertChunk == FALSE)) stop("there is still chunk in the data, try: channel.removeChunk")
}
