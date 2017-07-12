# --------------------------------------------------------------------------
# Code for cooking raw data
# authors: Daniel Klotz, Simon Frey, Johannes Wesemann
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# --------------------------------------------------------------------------

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

# - -----------------------------------------------------------------------

#' Load many libraries at once
#' 
#' @description 
#' \code{libraries} conveniently wraps the \code{\link{base::library}} function so 
#' that multiple packages can be loaded with a signle one function call. 
#' @author Simon Frey, Daniel Klotz, Christoph Schürz
#' 
#' 
#' @param ... Arguments passed to \code{\link{library}}.
#' @param vebrose A logical vector, indicating if the package start-up messages 
#'                   should be supressed (\code{FALSE}) or not (\code{TRUE}).
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

# - -----------------------------------------------------------------------

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
