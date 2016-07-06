#' Extract spinup time from stats file
#' 
#' A little wrapper to get information about the spinup position from a file. The spinup-time is supposed to be an integer, which marks the position in the data which can be cut away for various reasons (e.g. model spin up time).
#' The wrapper assumes that the infomration (integer) is stored in a file (filepath) and marked by pattern, e.g.: within the string "Pre-Amp-Time: 200" the frist part would be the `pattern`. 
#' 
#' @param filepath path to the given file
#' @param pattern string used to mark that the next integer is the spinup time
#' @return Integer indicating the length of the spin-up time in hours
#' @export
pour.spinup <- function(filepath,pattern) {
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
