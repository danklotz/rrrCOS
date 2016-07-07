assert_comple_data <- function(runoff_data) {
  OK_COSdate <- any(names(runoff_data)== viscos_options()$name_COSyear)
  OK_POSIXdates <- any(names(runoff_data)== viscos_options()$name_COSposix)
  # choose function depending on which formats are available!
  if (!OK_COSdate & !OK_POSIXdates) {
    stop("No COSdates and no POSIXct-dates in the data!")
  } else if (OK_COSdate & !OK_POSIXdates) { 
    stop("NO POSIXct fomrated column within the runoff_data!")
  } else if (!OK_COSdate & OK_POSIXdates) {
    stop("NO COSdate year within the runoff_data!")
  }
}