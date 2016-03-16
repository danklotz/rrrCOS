#§ not used anymore! Will be removed soon
# calculate hydrological years
# 
# @param runoff_data The data.frame, which contains the runoff information
# @return The runoff data.frame reduced and ordered according to the hydrological years within the data. 
# \strong{Note:} The hydrological years are formatted as characters.
# @export
prepare_hydyears <- function(runoff_data) {
  # pre 
  require(dplyr)
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  if ( !exists("POSIXdate", where = runoff_data) & !exists("yyyy", where = runoff_data) ) {
    stop("data.frame does neiter contain POSIXdate nor COSdate")
  } else if ( exists("POSIXdate", where = runoff_data) & !exists("yyyy", where = runoff_data) ) {
    stop("transformation from POSIXdate to COSdate not yet available :(")
  } else if ( !exists("POSIXdate", where = runoff_data) & exists("yyyy", where = runoff_data) ) {
    runoff_data <- implode_cosdate(runoff_data)
  }
  # calc
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