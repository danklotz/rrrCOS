#' order different data 
#' 
#' more descriptiuon will follow xxx (analougosly to )
#' @export
channel <- function(this, from_that) {
  # def
  input <- substitute(this)
  if ( is.character(input) ) {
    choice <- input 
  } else if (is.name(input)) {
    choice <- deparse(input)
  } else {
    stop( paste("Cannot fetch_ The option >>", what[1],"<< is neither a name nor a character!", sep = " ") )
  }
  # calc
  switch(choice, 
         completeDate = channel_complete_date(from_that), 
         hydyears = channel_hydyears(from_that),
         implode_cosdate = channel_implode_cosdate(from_that),
         onlyObserved = channel_only_observed(from_that),
         path = channel_path(from_that),
         remove_chunk = channel_remove_chunk(from_that),
         runoff_as_xts = channel_runoff_as_xts(from_that),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for channel", sep = " " ) )
  )
}