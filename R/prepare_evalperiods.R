#' calculate evaluation periods defined by user with start month and end month
#' 
#' depreciated!
#' @param runoff_data The data.frame, which contains the runoff information
#' @return The runoff data.frame, including a column indicating the evaluating period 
#' \strong{Note:} The hydrological years are formatted as characters.
#' @export
prepare_evalPeriods <- function(runoff_data, smonth, emonth) {
  # def
    require(magrittr, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
    runoff_data %<>% prepare_complete_date()
  # calc
  temp_runoff <- runoff_data
  stag <- 1
  evaltag <- 1
  for (i in 1:nrow(runoff_data)-1) {
    if (runoff_data$mm[i] < smonth & stag == 1) {
      temp_runoff <- temp_runoff[-1,]
    } else if (runoff_data$mm[i] >= smonth & stag == 1) {
    stag == 2
    evaltag <- evaltag
    runoff_data$EvalPeriod <- evaltag
    } else if (runoff_data$mm[i] >= smonth & stag == 1) {
      # corrected coding mistake : P
    }
  }
  #
  years <- pour_years_in_data(runoff_data)
  num_years = length(years$in_data)
  hydyears_in_d <- pour_hydyears(runoff_data,years)
  num_hydyears <- length(hydyears_in_d)
  # cut away data outside of hydyears (#§ bad solution, below is an idea for a better one?)
  runoff_data %<>% filter(yyyy > years$in_data[1] | mm >= 9 ) %>% 
    filter(yyyy < years$in_data[num_years] | mm < 9)
  # calculate and format hydrological years
  runoff_data$hydyear <- as.character(runoff_data$POSIXdate)
  for (i in 1:(num_hydyears)) 
  {
    runoff_data %<>% mutate(  hydyear = ifelse(
      (yyyy == years$in_data[i] & mm >= 9 ) | (yyyy == years$in_data[i+1] & mm <= 8 ),
      hydyears_in_d[i],
      hydyear)  ) 
  }
  return(runoff_data)
}

#§ solution so far is kinda bad, cause we throw data away. some maybe something like the style proposed in the following can be usefull?
#   g <- as.character(runoff_data$POSIXdate)
#   g[runoff_data$yyyy == years$in_data[1] & runoff_data$mm < 9] <- "pre"
#   g[runoff_data$yyyy == years$in_data[num_years] & runoff_data$mm > 9] <- "post"
#   runoff_data$hydyear <- g
#§
