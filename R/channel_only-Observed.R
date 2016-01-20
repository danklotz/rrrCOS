#' remove basins withouth observations
#'
#' Removes basins withouth observation (-999/NA values) away from the provided dataframe
#'
#' @param runoff_data A raw runoff_data data.frame, which may containts basins withouth observations.
#' \strong{Note:} It is assumed that all available basins are simulated!
#' @return data.frame without the observation-free basins
#' @export
channel.onlyObserved <- function(runoff_data) {
  # pre
  require(magrittr)
  assert.dataframe(runoff_data)
  assert.Chunk(runoff_data)
  # calc
  runoff_data[is.na(runoff_data)] <- -999 
  colmax <- lapply(X = runoff_data, FUN = max) # get max of any column
  if ( any(colmax == -999) ){
    # make shure that there are only qobs which have max -999
    idx_temp <- which(colmax == -999) 
    OnlyQobsSelected <- names(idx_temp) %>% tolower %>% grepl("qobs.*",.) %>% any
    if (!OnlyQobsSelected){
      stop("There are Qsim withouth simulation (i.e. only -999 values). Pls remove them first")
    }
    idx_slct <- c(idx_temp,idx_temp + 1) %>% sort() # we add +1 to the idx to get also the simulations
    d_onlyObserved <- runoff_data[-idx_slct]
    return(d_onlyObserved)
  } else {
    return(runoff_data)
  }
}
