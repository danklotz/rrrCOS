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
  # def
    #§ missing :(
    require(magrittr)
  # calc
  if (filepath == "dummy") {
    tmp <- c("a 100", "b 200", "c 300")
    spinup <- grep(pattern,tmp) %>%
      tmp[.] %>% 
      sub('\\D', '',.) %>% 
      as.integer(.) + 1
    if ( length(spinup) == 0 ) {
      stop("pattern or spinup-integer could not be found")
    }
    return(spinup)
  } else {
    tmp <- filepath %>% paste %>% readLines
    spinup <- grep(pattern,tmp) %>%
      tmp[.] %>% 
      sub('\\D', '',.) %>% 
      as.integer(.) + 1
    if ( length(spinup) == 0 ) {
      stop("pattern or spinup-integer could not be found")
    }
    return(spinup)
  }

}