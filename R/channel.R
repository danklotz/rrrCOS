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
    stop( paste("Cannot fetch. The option >>", what[1],"<< is neither a name nor a character!", sep = " ") )
  }
  # calc
  switch(choice, 
         completeDate = channel.complete_date(from_that), 
         hydyears = channel.hydyears(from_that),
         implode_cosdate = channel.implode_cosdate(from_that),
         onlyObserved = channel.onlyObserved(from_that),
         path = channel.path(from_that),
         remove_chunk = channel.remove_chunk(from_that),
         runoff_as_xts = channel.runoff_as_xts(from_that),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for channel", sep = " " ) )
  )
}