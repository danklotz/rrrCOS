#' get different data
#' 
#' The \code{pour} function is a wrapper around the pour_ functions. It works as following: 
#' \itemize{
#'  \item{ \code{pour(runoff_example)} } { Wraps: \code{\link[visCOS]{pour_runoff_example}} }
#'  \item{ \code{pour(spinup, filepath, pattern)} } { Wraps: \code{\link[visCOS]{pour_spinup}} }
#'  \item{ \code{pour(plt_ctrl)} } { Wraps: \code{\link[visCOS]{pour_plt_ctrl}} }
#'  }
#' @export
#' @examples 
#' # get some example data:
#' runoff_data <- pour(runoff_example) 
#' head(runoff_data)
#' # get regular expression for the runoff_data 
#' reg_expr <- pour(regex_runoff_data)
pour <- function(what, ...) {
  # def
    input <- substitute(what)
    if ( is.character(input) ) {
      choice <- input 
    } else if (is.name(input)) {
      choice <- deparse(input)
    } else {
      stop( paste("Cannot pour_ The option >>", what[1],"<< is neither a name nor a character!", sep = " ") )
    }
  # calc:
  switch(choice, 
         runoff_example = pour_runoff_example(),
         plt_ctrl = pour_plt_ctrl(),
         spinup = pour_spinup(...),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for pour", sep = " " ) )
         )
}
