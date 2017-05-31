#' check for unwanted columns:
assert_junk <- function(cos_data) {
  data_names <- names(cos_data)
  no_junk_cols <- get_regex_for_cos_data( ) %>% grepl(., data_names, ignore.case = TRUE)
  if (any(no_junk_cols == FALSE)) {
    unwanted_cols <- paste(data_names[!no_junk_cols], collapse = ", ")
    stop(
      paste("There are still unwanted columns in the data. Check:", 
            unwanted_cols, 
            collapse = " ")
            )
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
  library("tibble", quietly = TRUE)
  if ( !is.data.frame(data) & !is.tibble(data) ) stop("data needs to be a data_frame!")
}
# uses stop if the input: "data" is not of class "data.frame"
build_tibble <- function(data) {
  library("tibble", quietly = TRUE)
  if ( !is.data.frame(data) ) {
    data <- as_tibble(data)
  } 
  return(data)
}
