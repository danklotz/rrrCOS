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
  #colmax <- function(x) lapply(X = runoff_data, FUN = max) # get max values of each column
  #
  runoff_data[is.na(runoff_data)] <- -999
  colmax <- lapply(X = runoff_data, FUN = max)
  idx_temp <- which(colmax == -999) # check for unobserved basins
  # make shure that there are only qobs which have max -999
  OnlyQobsSelected <- names(idx_temp) %>% tolower  %>% grepl("qobs.*",.) %>% any
  if (!OnlyQobsSelected){
    stop("There are Qsim withouth simulation (i.e. only -999 values). Pls remove them first")
  }
  # add +1 to the idx to get also the simulations
  idx_slct <- c(idx_temp,idx_temp + 1) %>% sort()
  d_onlyObserved <- runoff_data[-idx_slct]
  return(d_onlyObserved)
}
