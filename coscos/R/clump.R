#' Aggregate cosdata by keys 
#' 
#' @description 
#' \code{clump} provides a wrapper around \code{dplyr::group_by} and 
#' \code{dplyr::summarise_at} that allows for a row-wise aggregation of 
#' \code{cosdata} by a set of chosen colums (\code{keys})
#' @author Daniel Klotz
#' 
#' 
#' @seealso \code{\link{cook_cosdata}}
#' @family cosdata manipulators
#' 
#' @importFrom dplyr group_by_ summarise_at
#' @importFrom tibble as_tibble
#' @export
clump <- function(cosdata, 
                  key = "mm", 
                  .funs = mean, 
                  opts = coscos::viscos_options()) {
  if(missing(key))
    stop(simpleError("Error: Key column must be defined!"))
  le_data <- cook_cosdata(cosdata)
  le_names <- names(le_data) 
  le_names %>% 
    grepl(paste(key, collapse = "|"), ., ignore.case = TRUE) %>% 
    any(.) %>% 
    if(!.) stop("The chosen key (" %&% key %&% ") is not in `cosdata`")
  # main: =================================================================
  data_names <- le_names[
    grepl(opts$name_o %|% opts$name_s, le_names, ignore.case = TRUE)]
  le_result <- dplyr::group_by_(le_data, .dots = key) %>% 
    dplyr::summarise_at(., .vars = data_names, .funs = .funs)
  return(  as_tibble(le_result)  )
}

# other aggregators -------------------------------------------------------

#' Aggregate posixdate by keys 
#' 
#' \code{clump_posix} emulates the \code{\link{clump}} function for the 
#' \code{posix} column.
#' 
#' 
#' @importFrom purrr map_chr
#' @export
clump_posix <- function(le_posix, 
                        key = "mm", 
                        opts = coscos::viscos_options()) {
  if(missing(key))
    stop(simpleError("Error: Key column must be defined!"))
  if( !all(class(le_posix) == c("POSIXct","POSIXt")) )
    stop('le_posix must be of class "POSIXct" "POSIXt"')
  # main: =================================================================
  # define bounds to cut the posix-string:
  str_bounds <- c(Inf,-Inf)
  if (  key %>% grepl("dd", ., ignore.case = TRUE) %>% any(.)  ) {
    str_bounds[1] <- min(9, str_bounds[1])
    str_bounds[2] <- max(11, str_bounds[2])
  }
  if (  key %>% grepl("mm", ., ignore.case = TRUE) %>% any(.)  ) {
    str_bounds[1] <- min(6, str_bounds[1])
    str_bounds[2] <- max(7, str_bounds[2])
    # interval_bounds <- c(min = cosdata[viscos_options("name_COSmonth")] %>% min(.),
    #                      max = cosdata[viscos_options("name_COSmonth")] %>% max(.))
  }
  if (  key %>% grepl("yyyy", ., ignore.case = TRUE) %>% any(.)  ) {
    str_bounds[1] <- min(1, str_bounds[1])
    str_bounds[2] <- max(4, str_bounds[2])
  }
  cut_str <- function(x) substr(x, str_bounds[1], str_bounds[2])
  eliminate_spaces <- function(chr) gsub(" ","",chr)
  le_result <- purrr::map_chr(le_posix, cut_str) %>% 
    purrr::map_chr(.,eliminate_spaces) %>% 
    unique(.) 
  return(le_result)
}