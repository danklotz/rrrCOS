#' test for Chunk 
#'  
#' tests if there is still chunk in the runoff_data data frame
#' @
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


#' test if everything is OK with the basic objective function list
#' 
#' returns error messages if the basicOF object is not a list or 
#' does not contain all the basic OFs or 
#' if it contains more then the basic OFs
testfor.basicOF <- function(basicOF) {
  if ( !is.list(basicOF) ) stop("The basic objective funcitons are not stored in a list!")
  testforRightOF <- any( grepl("NSE.*|KGE.*|pBIAS.*|CORR.*",names(basicOF)) == TRUE)
  if ( !tsetforRightOF ) stop("The basic objective function do not contain the right objective functions (see: xxx)") 
}