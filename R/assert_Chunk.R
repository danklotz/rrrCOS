#' test for Chunk 
#'  
#' tests if there is still chunk in the runoff_data data frame
assert.Chunk <- function(runoff_data) {
  require(magrittr)
  regEx <- fetch.runoff_dataRegEx()
  assertChunk <- names(runoff_data) %>% tolower %>% grepl(regEx,.) 
  if (any(assertChunk == FALSE)) stop("there is still chunk in the data, try: channel.removeChunk")
}