#' get different data
#' 
#' The \code{fetch} function is a wrapper around the fetch_ functions. It works as following: 
#' \itemize{
#'  \item{ \code{fetch(spinup,filepath, pattern)} } { Wraps: \code{\link[visCOS]{fetch_spinup}} }
#'  \item{ \code{fetch(number_of_basins,runoff_data)} } { Wraps: \code{\link[visCOS]{fetch_number_of_basins}} }
#'  \item{ \code{fetch(hydears,runoff_data, years_in_data)} } { Wraps: \code{\link[visCOS]{fetch_hydyears}} }
#'  \item{ \code{fetch(years_in_data, runoff_data)} } { Wraps: \code{\link[visCOS]{fetch_years_in_data}} }
#'  \item{ \code{fetch(runoff_as_xts, runoff_data)} } { Wraps: \code{\link[visCOS]{fetch_runoff_as_xts}} }
#'  }
#' @export
#' @examples 
#' # get some example data:
#' runoff_data <- fetch(runoff_example) 
#' head(runoff_data)
#' # 
#' # get number of basins: 
#' runoff_data <- fetch(runoff_example) 
#' fetch(number_of_basins,channel_removeChunk(runoff_data))
#' #
#' # get years in data 
#' runoff_data <- fetch(runoff_example) 
#' fetch(years_in_data,runoff_data)
fetch <- function(what, ...) {
  # def
  input <- substitute(what)
  if ( is.character(input) ) {
    choice <- input 
  } else if (is.name(input)) {
    choice <- deparse(input)
  } else {
    stop( paste("Cannot fetch_ The option >>", what[1],"<< is neither a name nor a character!", sep = " ") )
  }
  # 
  switch(choice, 
         hydyears = fetch_hydyears(...),
         number_of_basins = fetch_number_of_basins(runoff_data = ...),
         runoff_example = fetch_runoff_example(),
         some_ofun = fetch_some_ofun(...),
         some_ofun_4_hydyears = fetch_some_ofun_4_hydyears(...),
         spinup = fetch-spinup(...),
         years_in_data = fetch_years_in_data(runoff_data = ...),
         runoff_as_xts = fetch_runoff_as_xts(from_that),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for fetch", sep = " " ) )
         )
}
