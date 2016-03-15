# dive function let you explore data via shiny
###################################################################################################

# Dive With OF ------------------------------------------------------------
#' dive with ofun 
#' 
#' Runs a Shiny App which can be used to get an overview of a runoff_data time series object. 
#' Explore the runoff_data with a little [shiny](http://shiny.rstudio.com/) App. 
#' 
#' @param d_xts runoff_data formatted as time series
#' @export
#' @examples 
#' # get example data, 
#' # clean it and 
#' # explore the model performance
#' d_runoff <- channel_remove_chunk( pour_runoff_example() )
#' dive_runoff_with_ofun(d_runoff)
dive_runoff_with_ofun <- function(runoff_data) {
  # pre
  require("data.table", quietly = TRUE) 
  require("magrittr", quietly = TRUE)
  require("shiny", quietly = TRUE)
  ##########################
  # calc
  #$ this is all suboptimal, maybe exploit the global function or something
  runoff_data %<>% channel_names
  if ( !"POSIXdate" %in% names(runoff_data) ) {
    runoff_data$POSIXdate <- channel_implode_cosdate(runoff_data)
  }
  runoff_data <<- runoff_data
  d_xts <<- pour_runoff_as_xts(runoff_data)
  d_names_all<- names(d_xts)
  idx_names <- d_names_all %>% tolower %>% grepl("\\d" ,.)
  d_names <<- d_names_all[idx_names]
  d_nums <<- d_names %>% gsub("\\D","",.) %>% as.integer %>% unique 
  #
  runApp("R/AppExplore") #$ how do I fix the path to the app?
}










