# Get years of the runoff_data
#
# @return list with (short and long named) years. \strong{Note:} Short names years are just characters.
# Thus, they may only be usefull for plotting
# @export
pour_years_in_data <- function(runoff_data) {
  require(magrittr)
  if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
  # calc
  years <- list()
  years$in_data <- unique(runoff_data$yyyy)
  years$in_data_shrt <- years$in_data %>%
    unique %>%
    as.character %>%
    substring(.,3,4)
  return(years)
}
