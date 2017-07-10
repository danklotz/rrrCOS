#' Chop cosdata 
#' 
#' Divides `cosdata` into a tibble according to the unique idtentifiers of a 
#' given column.
#'
#' @import pasta
#' @export
chop <- function(cosdata, key) {
  if(missing(key))
    stop(simpleError("Error: Key column must be defined!"))
  le_data <- cook_cosdata(cosdata)
  key_is_in_data <- names(le_data) %>% 
    grepl(paste(key, collapse = "|"), ., ignore.case = TRUE) %>% 
    any(.)
  if( !key_is_in_data )
    stop("The chosen key (" %&% 
           key %&% 
           ") is not part of the `cosdata` columns")
  # 
  chop_symbols <- cosdata[key] %>% 
    unique(.) %>% 
    as.matrix(.)
  # make slices along the vertical dimension of le_data: ==================
  slices <- apply(chop_symbols, 
                  1, 
                  function(x) le_data[le_data[key] == x, ])
  # give the different slices appropriate names and return: ===============
  names(slices) <- "slice" %&% 1:length(slices) %_%  paste(key, collapse = "_") 
  return(slices)
}
