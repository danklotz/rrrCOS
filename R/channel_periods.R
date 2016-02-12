#' calculate periods defined by user 
#' @param runoff_data The data.frame, which contains the runoff information
#' @return The runoff data.frame reduced and ordered according to the hydrological years within the data. 
#' \strong{Note:} The hydrological years are formatted as characters.
#' @export
channel.periods <- function(runoff_data, start_month, end_month) {
  # pre 
    require(dplyr)
    if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
    if ( !exists("POSIXdate", where = runoff_data) & !exists("yyyy", where = runoff_data) ) {
      stop("data.frame does neiter contain POSIXdate nor COSdate")
    } else if ( exists("POSIXdate", where = runoff_data) & !exists("yyyy", where = runoff_data) ) {
      stop("transformation from POSIXdate to COSdate not yet available :(")
    } else if ( !exists("POSIXdate", where = runoff_data) & exists("yyyy", where = runoff_data) ) {
      runoff_data$POSIXdate <- channel.implode_cosdate(runoff_data)
    }
  # calc:
  # get labels for the monts
  if (start_month <= end_month ) {
    period_range <- seq(start_month,end_month)
  } else if (start_month > end_month) {
    range_1 <- seq(start_month,12)
    range_2 <- seq(1,end_month)
    period_range <- c(range_1,range_2)
  }
  # mark periods:
  eval_dif <- function(a) {c(a[1],diff(a))}
  start_sorter <- runoff_data$mm %in% c(start_month) %>% eval_dif %>% pmax(.,0)
  end_sorter <- runoff_data$mm %in% c(end_month) %>% eval_dif %>% pmin(.,0)
  start_points <- which(start_sorter == 1) 
  end_points <- which(end_sorter == -1)
  #
  runoff_data$period <- 0
  for ( k in 1:length(end_points) ) {
    period_idx <- seq(start_points[k],end_points[k])
    runoff_data$period %<>% replace(period_idx, k)
  }
  return(runoff_data)
}
