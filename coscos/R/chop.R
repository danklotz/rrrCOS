#' Chop cosdata 
#' 
#' @description 
#' \code{chop} divides \code{cosdata} into according to chosen column 
#' identifiers (\code{keys}). The result is a subdivided \code{list}  
#' which containts the different parts (i.e. rows) of the original data.
#' @author Daniel Klotz
#'
#' @param cosdata The strictly defined data format (\code{cosdata}) used 
#'      within \pkg{viscos} (see: \code{\link{cook_cosdata}}).
#' @param keys A \code{string} of one or more column-names that designate 
#'      the index-columns for the data separation.
#' 
#' @examples 
#' # separate the data with regard to the months: 
#' cosdata <- viscos_example()
#' chopped <- chop(cosdata, keys = c("mm"))
#' print(head(chopped$slice1_mm))
#' 
#' # separate the data with regard to the year and months: 
#' chopped <- chop(cosdata, keys = c("yyyy","mm"))
#' print(head(chopped$slice1_yyyy_mm))
#' 
#' @seealso \code{\link{cook_cosdata}}
#' @family cosdata manipulators
#'
#' @import pasta
#' @importFrom purrr map_dfc
#' @export
chop <- function(cosdata, keys) {
  if(missing(keys))
    stop(simpleError("Error: keys column must be defined!"))
  le_data <- cook_cosdata(cosdata)
  keys_are_in_data <- names(le_data) %>% 
    grepl(paste(keys, collapse = "|"), ., ignore.case = TRUE) %>% 
    any(.)
  if( !keys_are_in_data ) {
    stop("The chosen keys (" %&% 
           keys %&% 
           ") is not part of the `cosdata` columns")
  }
  # main: =================================================================
  chop_symbols <- cosdata[keys] %>% 
    unique(.) %>% 
    as.matrix(.)
  # make slices along the vertical dimension of le_data: ==================
  slices <- apply(
    chop_symbols,
    1, 
    function(x_symbol) {
      c_mask <- map2_dfc(x_symbol,keys, function(xx, key) le_data[key]==xx) %>% 
        apply(., 1, all)
      le_data[c_mask, ]
      }
    ) 

  # give the different slices appropriate names and return: ===============
  names(slices) <- "slice" %&% 1:length(slices) %_%  paste(keys, collapse = "_") 
  return(slices)
}
