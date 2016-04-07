#' Wraps the prepare functions 
#' 
#' prepare functions can be used to re-order the data so that it is in the necessary format for plotting. 
#' This function ('prepare') is a wrapper around the prepare. functions. The usage is defined as following
#' \itemize{
#'  \item{ \code{prepare(completeDate)} } { Wraps: \code{\link[visCOS]{prepare.complete_date}} }
#'  \item{ \code{prepare(periods)} } { Wraps: \code{\link[visCOS]{prepare.periods}} }
#'  \item{ \code{prepare(complete_date)} } { Wraps: \code{\link[visCOS]{prepare.complete_date}} }
#'  \item{ \code{prepare(only_observed)} } { Wraps: \code{\link[visCOS]{prepare.only_observed}} }
#'  \item{ \code{prepare(remove_chunk,runoff_data})} } { Wraps: \code{\link[visCOS]{prepare.remove_chunk}} }
#'  \item{ \code{prepare(runoff_as_xts, runoff_data)} } { Wraps: \code{\link[visCOS]{prepare.runoff_as_xts}} }
#'  }
#' @export
#' 
#' @examples 
#' # get runoff example and clean data, remove chunk and clean names
#' d_raw <- pour_runoff_example()
#' d_runoff <- prepare(remove_chunk, runoff_data)
#' names(d_raw)
#' names(d_runoff)

prepare <- function(this, from_that) {
  # def
  input <- substitute(this)
  if ( is.character(input) ) {
    choice <- input 
  } else if (is.name(input)) {
    choice <- deparse(input)
  } else {
    stop( paste("Cannot pour_ The option >>", what[1],"<< is neither a name nor a character!", sep = " ") )
  }
  # calc
  switch(choice, 
         completeDate = prepare.complete_date(from_that), 
         periods = prepare.periods(from_that),
         complete_date = prepare.complete_date(from_that),
         only_observed = prepare.only_observed(from_that),
         remove_chunk = prepare.remove_chunk(from_that),
         runoff_as_xts = prepare.runoff_as_xts(from_that),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for prepare", sep = " " ) )
  )
}
