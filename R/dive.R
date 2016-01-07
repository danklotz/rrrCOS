###################################################################################################
#' Explore runoff_data time series with shiny
#' 
#' Runs a Shiny App which can be used to get an overview of a runoff_data time series object
#' 
#' @param d_xts runoff_data formatted as time series
#' @export
explore.runoff <- function(d_xts) {
  ##########################
  # pre
  ##########################
  require("data.table") 
  require("magrittr")
  require("shiny")
  ##########################
  # defences
  ##########################
  
  ##########################
  # calc
  ##########################
  # get d_nums 
  names <- names(d_xts)
  names %<>% gsub('.*_', '' ,.) %>% unique(.) # replaces everything before "_" and gets the unique names
  d_nums <- names[!grepl("[a-z]",names)] %>% as.integer(.)
  
  runApp("R/AppExplore")
}