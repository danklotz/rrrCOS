#' Shiny App: Explore runoff_data time series
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
  d_names <- names(d_xts)
  d_names %<>% gsub('.*_', '' ,.) %>% unique(.) # replaces everything before "_" and gets the unique names
  d_nums <- d_names[!grepl("[a-z]",d_names)] %>% as.integer(.)
  # move variables into global environment for shiny #§ solution is so so
  d_names <<- d_names
  d_nums <<- d_nums 
  #
  runApp("R/AppExplore") #$ how do I fix the path to the app?

}