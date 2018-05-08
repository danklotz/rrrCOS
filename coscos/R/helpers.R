# --------------------------------------------------------------------------
# Helper Functions
# authors: Daniel Klotz, Simon Frey, Johannes Wesemann
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# --------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' Checks for cosdata
#' 
#' Perform a set of checks to guarantee tht the provided tht is in the 
#' `cosdata` format. 
#' 
#' @keywords internal
#' 
#' @import pasta
check_cosdata <- function(cosdata, opts = coscos::viscos_options()) {
  # check for names: 
  data_names <- names(cosdata)
  check_names <- grepl(regex1_all_cosdata(opts), 
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
  OK_COSdate <- any(names(cosdata) == opts["name_COSyear"])
  OK_POSIXdates <- any(names(cosdata) == opts["name_COSposix"])
  # choose error messag depending on which columns are missing!
  if (!OK_COSdate & !OK_POSIXdates) {
    stop("No COSdates and no POSIXct-dates in the data!")
  } else if (OK_COSdate & !OK_POSIXdates) {
    stop("NO POSIXct fomrated column within the cosdata!")
  } else if (!OK_COSdate & OK_POSIXdates) {
    stop("NO COSdate year within the cosdata!")
  }
  # check correct data types: 
  # first get classes then test all fixed columns:
  le_classes <- lapply(cosdata,class)
  class_tests <- rep(FALSE,8)
  names(class_tests) <- c(
    opts["name_COSyear"],
    opts["name_COSmonth"],
    opts["name_COSday"],
    opts["name_COShour"],
    opts["name_COSmin"],
    opts["name_COSperiod"],
    opts["name_COSposix"],
    "data_columns"
  )
  class_tests[1] <- le_classes[[ opts[["name_COSyear"]] ]] == "integer"
  class_tests[2] <- le_classes[[ opts[["name_COSmonth"]] ]] == "integer"
  class_tests[3] <- le_classes[[ opts[["name_COSday"]] ]] == "integer"
  class_tests[4] <- le_classes[[ opts[["name_COShour"]] ]] == "integer"
  class_tests[5] <- le_classes[[ opts[["name_COSmin"]] ]] == "integer"
  class_tests[6] <- le_classes[[ opts[["name_COSperiod"]] ]] == "integer"
  class_tests[7] <- all(le_classes[[ opts[["name_COSposix"]] ]] == c("POSIXct","POSIXt"))
  # set all checked list-entries to NULL to remove them:
  le_classes[[ opts[["name_COSyear"]] ]] <- NULL
  le_classes[[ opts[["name_COSmonth"]] ]] <- NULL
  le_classes[[ opts[["name_COSday"]] ]] <- NULL
  le_classes[[ opts[["name_COShour"]] ]] <- NULL
  le_classes[[ opts[["name_COSmin"]] ]] <- NULL
  le_classes[[ opts[["name_COSperiod"]] ]] <- NULL
  le_classes[[ opts[["name_COSposix"]] ]] <- NULL
  # check if reminaing columns are all numeric: 
  class_tests[8] <- all( sapply(le_classes, function(x) x == "numeric") )
  if ( any(class_tests == FALSE) ) {
    error_message = "One of more columns have the wrong class/type. \n" %&%
      "Tests failed are associated the following columns:  \n" %&%
      "     " %&% paste(names(class_tests)[class_tests], collapse = ", ")
    stop(error_message)
  }
  return(NULL)
}


# -------------------------------------------------------------------------
#' Build tibble 
#' 
#' Transfroms dta into  tibble if it isn't one already
#' 
#' @keywords internal
#' 
#' @import tibble
#' @export 
build_tibble <- function(data) {
  if ( !tibble::is_tibble(data) ) {
    data <- tibble::as_tibble(data)
  } 
  return(data)
}


#' Regular Expressions Presets 1
#' 
#' A set of preset regular expressions to search for all cosdata columns. 
#'  
#' @keywords internal
regex1_all_cosdata <- function(opts) {
  regex_pattern <- paste("^",opts["name_COSyear"],"$|",
                         "^",opts["name_COSmonth"],"$|",
                         "^",opts["name_COSday"],"$|",
                         "^",opts["name_COShour"],"$|",
                         "^",opts["name_COSmin"],"$|",
                         opts["name_o"],".*|",
                         opts["name_s"],".*|",
                         opts["name_lb"],".*|",
                         opts["name_ub"],".*|",
                         opts["name_COSposix"],"|",
                         opts["name_COSperiod"],
                         sep = "")
  return(regex_pattern)
}


# -------------------------------------------------------------------------
#' Load multiple libraries
#' 
#' @description 
#' \code{libraries} conveniently wraps the \code{\link{base::library}} function so 
#' that multiple packages can be loaded with a signle one function call. 
#' @author Simon Frey, Daniel Klotz
#' 
#' 
#' @param ... Arguments passed to \code{\link{library}}.
#' @param vebrose A logical vector, indicating if the package start-up messages 
#'                   should be supressed (\code{FALSE}) or not (\code{TRUE}).
#' 
#' @examples
#' # loading can be done by string or by directly insertin an expression: 
#' libraries(xts)
#' libraries(shiny)
#' 
#' # load multiple libraries: 
#' libraries("xts","shiny")
#' libraries(xts, shiny)
#' 
#' # mixed arguments are also possible:
#' libraries(magrittr,"shiny","png",stats) 
#' 
#' @return Returns a logical vector that informas if the given package could be 
#'            loaded
#' @seealso \code{\link{require}}, \code{\link{library}},
#'          \code{\link{install.packages}} 
#' 
#' @importFrom lazyeval lazy_dots
#' @importFrom purrr map_lgl
#' 
#' @export
libraries <- function(..., verbose = FALSE) {
  if (base::is.logical(verbose) != TRUE) 
    stop( "verboes must be a logical! it currently is:" %&&% class(verbose) )
  parse_string <- ifelse(verbose,
                         "library('le_package', logical.return = TRUE)", 
                         "suppressPackageStartupMessages(library('le_package', 
                              logical.return = TRUE))")
  le_dots <- lazyeval::lazy_dots(...)
  loop_length <- length(le_dots)
  loader <- function(x) {
    le_package <- as.character(le_dots[[x]]$expr)
    package_loaded <- eval(
      parse( text = gsub("le_package",le_package,parse_string) )
      )
  }
  le_result <- purrr::map_lgl(1:loop_length, loader)
}


# -------------------------------------------------------------------------
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
