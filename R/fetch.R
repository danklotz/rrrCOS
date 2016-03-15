#' get different data
#' 
#' The \code{pour} function is a wrapper around the pour_ functions. It works as following: 
#' \itemize{
#'  \item{ \code{pour(spinup,filepath, pattern)} } { Wraps: \code{\link[visCOS]{pour_spinup}} }
#'  \item{ \code{pour(number_of_basins,runoff_data)} } { Wraps: \code{\link[visCOS]{pour_number_of_basins}} }
#'  \item{ \code{pour(hydears,runoff_data, years_in_data)} } { Wraps: \code{\link[visCOS]{pour_hydyears}} }
#'  \item{ \code{pour(years_in_data, runoff_data)} } { Wraps: \code{\link[visCOS]{pour_years_in_data}} }
#'  \item{ \code{pour(runoff_as_xts, runoff_data)} } { Wraps: \code{\link[visCOS]{pour_runoff_as_xts}} }
#'  }
#' @export
#' @examples 
#' # get some example data:
#' runoff_data <- pour(runoff_example) 
#' head(runoff_data)
#' # 
#' # get number of basins: 
#' runoff_data <- pour(runoff_example) 
#' pour(number_of_basins,channel_removeChunk(runoff_data))
#' #
#' # get years in data 
#' runoff_data <- pour(runoff_example) 
#' pour(years_in_data,runoff_data)
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
         hydyears = pour_hydyears(...),
         number_of_basins = pour_number_of_basins(runoff_data = ...),
         runoff_example = pour_runoff_example(),
         some_ofun = pour_some_ofun(...),
         some_ofun_4_hydyears = pour_some_ofun_4_hydyears(...),
         spinup = pour_spinup(...),
         years_in_data = pour_years_in_data(runoff_data = ...),
         runoff_as_xts = pour_runoff_as_xts(from_that),
         stop( paste("The option >>",what[1],"<< does not exist as a selection for pour", sep = " " ) )
         )
}
