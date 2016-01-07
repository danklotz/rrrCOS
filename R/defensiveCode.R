#' test for Chunk 
#'  
#' tests if there is still chunk in the runoff_data data frame
testfor.Chunk <- function(runoff_data) {
  require(magrittr)
  regEx <- fetch.runoff_dataRegEx()
  testForChunk <- names(runoff_data) %>% tolower %>% grepl(regEx,.) 
  if (any(testForChunk == FALSE)) stop("there is still chunk in the data, try: channel.removeChunk")
}

#' test if data is a data frame
#' 
#' returns error message if the input: "data" is not of class "data.frame" 
testfor.dataframe <- function(data) {
  if ( !is.data.frame(data) ) stop("runoff_data is no data_frame!")
}
