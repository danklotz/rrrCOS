#' @import magrittr
assert_junk <- function(cos_data) {
  regEx <- get_regex_for_cos_data()
  assertChunk <- names(cos_data) %>% grepl(regEx, ., ignore.case = TRUE)
  if (any(assertChunk == FALSE)) {
    stop("there is still unwanted columns in the data. Try: remove_junk")
  }
}
assert_complete_date <- function(cos_data) {
  OK_COSdate <- any(names(cos_data) == viscos_options("name_COSyear"))
  OK_POSIXdates <- any(names(cos_data) == viscos_options("name_COSposix"))
  # choose error messag depending on which columns are missing!
  if (!OK_COSdate & !OK_POSIXdates) {
    stop("No COSdates and no POSIXct-dates in the data!")
  } else if (OK_COSdate & !OK_POSIXdates) {
    stop("NO POSIXct fomrated column within the cos_data!")
  } else if (!OK_COSdate & OK_POSIXdates) {
    stop("NO COSdate year within the cos_data!")
  }
}
# uses stop if the input: "data" is not of class "data.frame"
assert_dataframe <- function(data) {
  require("tibble", quietly = TRUE)
  if ( !is.data.frame(data)&!is.tibble(data) ) stop("data needs to be a data_frame!")
}
