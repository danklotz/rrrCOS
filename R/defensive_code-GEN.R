assert_chunk <- function(runoff_data) {
  require(magrittr, quietly = TRUE)
  regEx <- get_regex_for_runoff_data()
  assertChunk <- names(runoff_data) %>% tolower %>% grepl(regEx,.)
  if (any(assertChunk == FALSE)) {
    stop("there is still unwanted columns in the data. Try: remove_chunk")
  }

}
assert_comple_date <- function(runoff_data) {
  OK_COSdate <- any(names(runoff_data)== viscos_options()$name_COSyear)
  OK_POSIXdates <- any(names(runoff_data)== viscos_options()$name_COSposix)
  # choose error messag depending on which columns are missing!
  if (!OK_COSdate & !OK_POSIXdates) {
    stop("No COSdates and no POSIXct-dates in the data!")
  } else if (OK_COSdate & !OK_POSIXdates) { 
    stop("NO POSIXct fomrated column within the runoff_data!")
  } else if (!OK_COSdate & OK_POSIXdates) {
    stop("NO COSdate year within the runoff_data!")
  }
}
# uses stop if the input: "data" is not of class "data.frame" 
assert_dataframe <- function(data) {
  if ( !is.data.frame(data) ) stop("runoff_data is no data_frame!")
}
assert_of<- function(of) {
  if ( !is.list(of) ) {
    stop("The basic objective funcitons are not stored in a list!")
  }
  assert_right_of <- any( grepl("NSE.*|KGE.*|pBIAS.*|CORR.*",names(of)) == TRUE)
  if ( !assert_right_of ) {
    stop("The objective functions do not contain the right named entries, i.e. NSE,KGE,pBIAS,CORR") 
  }
}
