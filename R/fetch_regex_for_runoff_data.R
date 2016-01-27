#' RegEx Pattern for runoff_data
#'
#' returns the regular expression needed to check if there is Chunk in the the runoff_data data.frame(see: xxx)
fetch.regex_for_runoff_data <- function() {
  RegExPattern  <- "^yyyy$|^mm$|^dd$|^hh$|^min$|qobs.*|qsim.*|posixdate|hydyear"
  return(RegExPattern)
}
