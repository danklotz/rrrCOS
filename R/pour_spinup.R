#' Extract spinup time from stats file
#' 
#' Spinup-time is an integer. It is the time in hours, which is not used for the model-evalution. 
#' The pattern is the used marker within the file
#' 
#' 
#' @param filepath path to the given file
#' @param pattern string used to mark that the next integer is the spinup time
#' @return Integer indicating the length of the spin-up time in hours
#' @export
pour_spinup <- function(filepath,pattern) {
  # pre 
  require(magrittr)
  # 
  tmp <- filepath %>% paste %>% readLines
  spinup <- grep(pattern,tmp) %>%
    tmp[.] %>% 
    sub('.*:', '',.) %>% 
    as.integer(.) + 1
  return(spinup)
}