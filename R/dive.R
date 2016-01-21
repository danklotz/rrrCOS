# dive function let you explore data via shiny
###################################################################################################

# Dive With OF ------------------------------------------------------------
#' Explore runoff_data time series with shiny
#' 
#' Runs a Shiny App which can be used to get an overview of a runoff_data time series object
#' 
#' @param d_xts runoff_data formatted as time series
#' @export
explore.runoff <- function(d_xts) {
  # pre
  require("data.table") 
  require("magrittr")
  require("shiny")
  ##########################
  # calc
  d_names <- names(d_xts)
  idx_names <- d_names %>% tolower %>% grepl("qobs.*|qsim.*" ,.)
  d_names <<- d_names[idx_names]
  d_nums <<- d_xts %>% as.data.frame %>% fetch(.,number_of_basins) 
  #
  runApp("R/AppExplore") #$ how do I fix the path to the app?
}










