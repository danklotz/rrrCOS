#' get different data
#' 
#' @export
#' @examples 
#' runoff_data <- fetch(runoff_example) 
#' head(runoff_data)
#' fetch(number_of_basins,channel.removeChunk(runoff_data))
fetch <- function(what, ...) {
  # def
  input <- substitute(what)
  if ( is.character(input) ) {
    choice <- input 
  } else if (is.name(input)) {
    choice <- deparse(input)
  } else {
    stop( paste("Cannot fetch. The option >>", what[1],"<< is neither a name nor a character!", sep = " ") )
  }
  # 
  switch(choice, 
         number_of_basins = fetch.number_of_basins(...),
         runoff_example = fetch.runoff_example(),
         ctrl = fetch.ctrl(), 
         hydyears = fetch.hydyears(...),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for fetch", sep = " " ) )
         )
}