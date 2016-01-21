#' get different data
#' 
#' @export
#' @examples 
#' runoff_data <- fetch(what = "runoff_example)
#' number_of_basins <- fetch(runoff_data, "number_of_basins")
fetch <- function(what, ...) {
  # def
  #assert.Chunk(runoff_data)
  #assert.dataframe(runoff_data)
  
  # 
  switch(what, 
         number_of_basins = fetch.number_of_basins(...),
         runoff_example = fetch.runoff_example(),
         stop( paste("The option >>",what,"<< does not exist as a selection for fetch", sep = " " ) )
         )
}