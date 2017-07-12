#' pre set regular expressions to serch for cosdta column 
#' @keywords internal
regex1_all_cosdata <- function(opts) 
  {
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


#' Loading many libraries at once
#' 
#' a conveince functions that wraps the `library` function so that 
#' multiple packages can be loaded with one function call. 
#' 
#' @param x character vector. Name(s) of the libraries that are loaded/installed.
#' @param ... Arguments passed to \code{\link{require}} and \code{\link{install.packages}}
#' @author Simon Frey
#' @description This function tries to load more than one package at once.
#' If any of these packages is not installed it tries to istall them and load them afterward.
#' 
#' @examples
#' # loading can be done by string or by directly insertin an expression: 
#' libraries(xts)
#' libraries(shin)
#' 
#' # load multiple libraries: 
#' libraries("xts","shiny")
#' libraries(xts, shiny)
#' 
#' # mixed arguments are also possible:
#' libraries(magrittr,"shiny","png",jpg) 
#' 
#' @return Returns nothing but gives a warning if it cannot load/install a library/package
#' @seealso \code{\link{require}}, \code{\link{library}}, \code{\link{install.packages}} 
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
    

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
