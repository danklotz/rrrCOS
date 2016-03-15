#' Wraps the channel functions 
#' 
#' channel_ functions can be used to re-order the data so that it is in the necessary format for plotting. 
#' This function ('channel') is a wrapper around the channel_ functions. The usage is defined as following
#' \itemize{
#'  \item{ \code{channel(completeDate)} } { Wraps: \code{\link[visCOS]{channel_complete_date}} }
#'  \item{ \code{channel(periods)} } { Wraps: \code{\link[visCOS]{channel_periods}} }
#'  \item{ \code{channel(implode_cosdate)} } { Wraps: \code{\link[visCOS]{channel_implode_cosdate}} }
#'  \item{ \code{channel(only_observed)} } { Wraps: \code{\link[visCOS]{channel_only_observed}} }
#'  \item{ \code{channel(path)} } { Wraps: \code{\link[visCOS]{channel_path}} }
#'  \item{ \code{channel(remove_chunk,runoff_data})} } { Wraps: \code{\link[visCOS]{channel_remove_chunk}} }
#'  }
#' @export
#' 
#' @examples 
#' # get runoff example and clean data, remove chunk and clean names
#' d_raw <- pour_runoff_example()
#' d_runoff <- channel(remove_chunk, runoff_data)
#' names(d_raw)
#' names(d_runoff)

channel <- function(this, from_that) {
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
         completeDate = channel_complete_date(from_that), 
         periods = channel_periods(from_that),
         implode_cosdate = channel_implode_cosdate(from_that),
         only_observed = channel_only_observed(from_that),
         path = channel_path(from_that),
         remove_chunk = channel_remove_chunk(from_that),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for channel", sep = " " ) )
  )
}
