#' RegEx Pattern for runoff_data
#'  
#' returns the regular expression needed to check if there is Chunk in the the runoff_data data.frame(see: xxx) 
fetch.runoff_dataRegEx <- function() {
  RegExPattern  <- "^yyyy$|^mm$|^dd$|^hh$|^min$|qobs.*|qsim.*|posixdate|hydyear"
  return(RegExPattern)
}