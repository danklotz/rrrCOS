#' Wraps the prepare functions 
#' 
#' prepare can be used to format your data such that it is in the necessary format for the other visCOS functions. 
#' This wrapper combines the single prepare functions. In total the folling functions are available: 
#' \itemize{
#'  \item{ 1 } { \code{\link[visCOS]{prepare.complete_date}} }
#'  \item{ 2 } { \code{\link[visCOS]{prepare.periods}} }
#'  \item{ 3 } { \code{\link[visCOS]{prepare.complete_date}} }
#'  \item{ 4 } { \code{\link[visCOS]{prepare.only_observed}} }
#'  \item{ 5 } { \code{\link[visCOS]{prepare.remove_chunk}} }
#'  \item{ 6 } { \code{\link[visCOS]{prepare.runoff_as_xts}} }
#'  }
#' @export
#' 
#' @examples 
#' # get runoff example and clean data, remove chunk and clean names
#' d_raw <- pour.runoff_example()
#' d_runoff <- prepare(remove_chunk, runoff_data)
#' names(d_raw)
#' names(d_runoff)

prepare <- function(runoff_data, period_start_month = 9, period_end_month = 8) {
  # def
    require("magrittr", quietly = TRUE)
    if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
    #§ missing
    formated_runoff_data <- runoff_data %>% 
      prepare.remove_chunk %>% 
      prepare.only_observed %>% 
      prepare.complete_date %>% 
      prepare.periods(., 
                      start_month = period_start_month, 
                      end_month = period_end_month)
    return(formated_runoff_data)
  # calc

}
