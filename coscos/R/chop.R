#' Chop cosdata 
#' 
#' \code{chop} divides \code{cosdata} into according to chosen column 
#' identifiers (\code{keys}). The result is a subdivided \code{list}  
#' which containts the different parts (i.e. rows) of the original data.
#'
#' @param cosdata The strictly defined data format (\code{cosdata}) used 
#'      within \code{viscos} (see: \code{\link{cook_cosdata}}).
#' @param keys A \code{string} of one or more column-names that designate 
#'      the index-columns for the data separation.
#' 
#' @examples 
#' separate the data with regard to the months: 
#' chopped <- chop(cosdata, key = c("mm"))
#' 
#' separate the data with regard to the year and months: 
#' chopped <- chop(cosdata, key = c("yyyy","mm"))
#' 
#' @family cosdata manipulators
#'
#' @import pasta
#' @export
chop <- function(cosdata, keys) {
  if(missing(keys))
    stop(simpleError("Error: keys column must be defined!"))
  le_data <- cook_cosdata(cosdata)
  keys_is_in_data <- names(le_data) %>% 
    grepl(paste(keys, collapse = "|"), ., ignore.case = TRUE) %>% 
    any(.)
  if( !keys_is_in_data ) {
    stop("The chosen keys (" %&% 
           keys %&% 
           ") is not part of the `cosdata` columns")
  }
  # main: =================================================================
  chop_symbols <- cosdata[keys] %>% 
    unique(.) %>% 
    as.matrix(.)
  # make slices along the vertical dimension of le_data: ==================
  slices <- apply(chop_symbols, 
                  1, 
                  function(x) le_data[le_data[keys] == x, ])
  # give the different slices appropriate names and return: ===============
  names(slices) <- "slice" %&% 1:length(slices) %_%  paste(keys, collapse = "_") 
  return(slices)
}
