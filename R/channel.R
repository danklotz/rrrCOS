#' Wraps the prepare functions 
#' 
#' prepare functions can be used to re-order the data so that it is in the necessary format for plotting. 
#' This function ('prepare') is a wrapper around the prepare_ functions. The usage is defined as following
#' \itemize{
#'  \item{ \code{prepare(completeDate)} } { Wraps: \code{\link[visCOS]{prepare_complete_date}} }
#'  \item{ \code{prepare(periods)} } { Wraps: \code{\link[visCOS]{prepare_periods}} }
#'  \item{ \code{prepare(implode_cosdate)} } { Wraps: \code{\link[visCOS]{prepare_implode_cosdate}} }
#'  \item{ \code{prepare(only_observed)} } { Wraps: \code{\link[visCOS]{prepare_only_observed}} }
#'  \item{ \code{prepare(path)} } { Wraps: \code{\link[visCOS]{prepare_path}} }
#'  \item{ \code{prepare(remove_chunk,runoff_data})} } { Wraps: \code{\link[visCOS]{prepare_remove_chunk}} }
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
         completeDate = prepare_complete_date(from_that), 
         periods = prepare_periods(from_that),
         implode_cosdate = prepare_implode_cosdate(from_that),
         only_observed = prepare_only_observed(from_that),
         path = prepare_path(from_that),
         remove_chunk = prepare_remove_chunk(from_that),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for prepare", sep = " " ) )
  )
}
