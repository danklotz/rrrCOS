#' @import magrittr
assert_chunk <- function(runoff_data) {
  regEx <- get_regex_for_runoff_data()
  assertChunk <- names(runoff_data) %>% tolower %>% grepl(regEx,.)
  if (any(assertChunk == FALSE)) {
    stop("there is still unwanted columns in the data. Try: remove_chunk")
  }
}
assert_complete_date <- function(runoff_data) {
  OK_COSdate <- any(names(runoff_data) == viscos_options("name_COSyear"))
  OK_POSIXdates <- any(names(runoff_data) == viscos_options("name_COSposix"))
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
  require("tibble", quietly = TRUE)
  if ( !is.data.frame(data)&!is.tibble(data) ) stop("data needs to be a data_frame!")
}
