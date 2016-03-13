# dive function let you explore data via shiny
###################################################################################################

# Dive With OF ------------------------------------------------------------
#' dive with ofun 
#' Runs a Shiny App which can be used to get an overview of a runoff_data time series object. 
#' Explore the runoff_data with a little [shiny](http://shiny.rstudio.com/) App. 
#' 
#' @param d_xts runoff_data formatted as time series
#' @export
dive_runoff_with_ofun <- function(runoff_data) {
  # pre
  require("data.table", quietly = TRUE) 
  require("magrittr", quietly = TRUE)
  require("shiny", quietly = TRUE)
  ##########################
  # calc
  d_xts <<- channel_runoff_as_xts(runoff_data)
  d_names <- names(d_xts)
  idx_names <- d_names %>% tolower %>% grepl("\\d" ,.)
  d_names <<- d_names[idx_names]
  d_nums <<- d_names %>% gsub("\\D","",.) %>% as.integer %>% unique 
  #
  runApp("R/AppExplore") #$ how do I fix the path to the app?
}










