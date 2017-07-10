  #' @import pasta
  check_cosdata <- function(cosdata) {
    # check for names: 
      data_names <- names(cosdata)
      check_names <- grepl(get_regex_for_cos_data(),
                           data_names,
                           ignore.case = TRUE)
      if( any(check_names == FALSE) ) {
        error_message <- "Cannot remove all unwanted columns." %&&%
                         "Try to remove them manually:" %&&%
                         "\n        " %&&%
          paste(data_names[1 - check_names], collapse = ", ")
        stop(error_message)
      } 
    # check for dimensions:
      n_cols <- ncol(cosdata)
      n_data_cols <- n_cols - 7 # 7 == amount of other columns in data 
      n_date_cols <- n_cols - n_data_cols - 2 # must be 5: yyyy,mm,dd,hh,min
      if ((n_data_cols %% 2) != 0) {
        stop("the number of o- and s-columns must be equal!")
        }
      if ((n_cols - n_data_cols - 2) != 5) {
        stop("cos-date columns are not complete")
      }
    # check for complete dates: 
      OK_COSdate <- any(names(cosdata) == viscos_options("name_COSyear"))
      OK_POSIXdates <- any(names(cosdata) == viscos_options("name_COSposix"))
      # choose error messag depending on which columns are missing!
      if (!OK_COSdate & !OK_POSIXdates) {
        stop("No COSdates and no POSIXct-dates in the data!")
      } else if (OK_COSdate & !OK_POSIXdates) {
        stop("NO POSIXct fomrated column within the cos_data!")
      } else if (!OK_COSdate & OK_POSIXdates) {
        stop("NO COSdate year within the cos_data!")
      }
    # check correct data types: 
      # first get classes then test all fixed columns:
      le_classes <- lapply(cosdata,class)
      class_tests <- rep(FALSE,8)
      names(class_tests) <- c(
        viscos_options("name_COSyear"),
        viscos_options("name_COSmonth"),
        viscos_options("name_COSday"),
        viscos_options("name_COShour"),
        viscos_options("name_COSmin"),
        viscos_options("name_COSperiod"),
        viscos_options("name_COSposix"),
        "data_columns"
        )
      class_tests[1] <- le_classes[[viscos_options("name_COSyear")]] == "integer"
      class_tests[2] <- le_classes[[viscos_options("name_COSmonth")]] == "integer"
      class_tests[3] <- le_classes[[viscos_options("name_COSday")]] == "integer"
      class_tests[4] <- le_classes[[viscos_options("name_COShour")]] == "integer"
      class_tests[5] <- le_classes[[viscos_options("name_COSmin")]] == "integer"
      class_tests[6] <- le_classes[[viscos_options("name_COSperiod")]] == "integer"
      class_tests[7] <- all(le_classes[[viscos_options("name_COSposix")]] == c("POSIXct","POSIXt"))
      # set all checked list-entries to NULL to remove them:
      le_classes[[viscos_options("name_COSyear")]] <- NULL
      le_classes[[viscos_options("name_COSmonth")]] <- NULL
      le_classes[[viscos_options("name_COSday")]] <- NULL
      le_classes[[viscos_options("name_COShour")]] <- NULL
      le_classes[[viscos_options("name_COSmin")]] <- NULL
      le_classes[[viscos_options("name_COSperiod")]] <- NULL
      le_classes[[viscos_options("name_COSposix")]] <- NULL
      # check if reminaing columns are all numeric: 
      class_tests[8] <- all(sapply(le_classes, function(x) x == "numeric") )
      if ( any(class_tests == FALSE) ) {
        error_message = "One of more columns have the wrong class/type. \n" %&%
                        "Tests failed are associated the following columns:  \n" %&%
                        "     " %&% paste(names(class_tests)[class_tests], collapse = ", ")
        stop(error_message)
      }
      return(NULL)
  }
# uses stop if the input: "data" is not of class "data.frame"
assert_dataframe <- function(data) {
  library("tibble", quietly = TRUE)
  if ( !is.data.frame(data) ) stop("data needs to be a data_frame!")
}
# uses stop if the input: "data" is not of class "data.frame"
build_tibble <- function(data) {
  library("tibble", quietly = TRUE)
  if ( !is.data.frame(data) ) {
    data <- as_tibble(data)
  } 
  return(data)
}
