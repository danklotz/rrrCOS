#' Aggregate or clump by keys 
#' 
#' @description 
#' An aggregate function, which wraps the \code{dplyr} functions \code{group_by} and \code{summarise_at} for a row-wise summary of the \emph{cosdata} \code{tibble}. It is defined by the set of functions given by \code{.funs} variable, and the \code{keys} string. The latter specifies the columns, according to whose unique entries the row-wise aggregation is conducted.
#' @author Daniel Klotz
#' 
#' @return An aggregated \code{tibble}.
#' @param .funs A aggregation function or list of aggregation functions.
#' @param cosdata The \emph{cosdata} \code{tibble} (see: \code{\link{cook_cosdata}}).
#' @param keys A \code{string}, indicating one or more column-names for data aggregation.
#' 
#' @examples 
#' # aggregate according to month
#' cosdata <- viscos_example()
#' clump(cosdata, key = "mm", .funs = mean)
#' 
#' # aggregate according to year and month
#' cosdata <- viscos_example()
#' clump(cosdata, key = c("yyyy", "mm"), .funs = median)
#' 
#' # aggregate with two functions according to year 
#' cosdata <- viscos_example()
#' clump( cosdata, key = "yyyy", .funs = list(a = mean, b = median) )
#' 
#' @seealso \code{\link{cook_cosdata}}
#' @family cosdata manipulators
#' 
#' @importFrom dplyr group_by_ summarise_at
#' @export
clump <- function(cosdata, 
                  keys = "mm", 
                  .funs = mean, 
                  opts = coscos::viscos_options()) {
  if(missing(keys))
    stop(simpleError("Error: keys column must be defined!"))
  le_data <- cook_cosdata(cosdata)
  le_names <- names(le_data) 
  le_names %>% 
    grepl(paste(keys, collapse = "|"), ., ignore.case = TRUE) %>% 
    any(.) %>% 
    if(!.) stop("The chosen keys (" %&% keys %&% ") is not in `cosdata`")
  # main: =================================================================
  data_names <- le_names[
    grepl(opts$name_o %|% opts$name_s, le_names, ignore.case = TRUE)]
  le_result <- dplyr::group_by_(le_data, .dots = keys) %>% 
    dplyr::summarise_at(., .vars = data_names, .funs = .funs)
  return(  build_tibble(le_result)  )
}

# other aggregators -------------------------------------------------------

#' Aggregate posixdate by keys 
#' 
#' Mirrors the \code{\link{clump}} function, but is used for the \code{posix} column only (see: \code{\link{cook_cosdata}}).
#' 
#' @return A character vector with the aggregated date-entries.
#' @param keys A \code{string} that contains the short-hand defintions for the aggregation. The used notation is: \code{yyyy} for \emph{years}, \code{mm} for \emph{months} and \code{dd} for \emph{days}.
#' 
#' @examples 
#' # clump dates by month
#' cosdata <- viscos_example() %>% cook_cosdata
#' clump_posix(cosdata$posixdate, keys = "mm")
#' 
#' # clump dates by years
#' cosdata <- viscos_example() %>% cook_cosdata
#' clump_posix(cosdata$posixdate, keys = "yyyy")
#' 
#' # clump dates by years and months
#' cosdata <- viscos_example() %>% cook_cosdata
#' clump_posix(cosdata$posixdate, keys = c("yyyy","mm"))
#' 
#' @importFrom purrr map_chr
#' @export
clump_posix <- function(le_posix, 
                        keys = "mm") {
  if(missing(keys))
    stop(simpleError("Error: 'keys' column must be defined!"))
  if( !all(class(le_posix) == c("POSIXct","POSIXt")) )
    stop('le_posix must be of class "POSIXct" "POSIXt"')
  # main: =================================================================
  # define bounds to cut the posix-string:
  str_bounds <- c(Inf,-Inf)
  if (  keys %>% grepl("dd", ., ignore.case = TRUE) %>% any(.)  ) {
    str_bounds[1] <- min(9, str_bounds[1])
    str_bounds[2] <- max(11, str_bounds[2])
  }
  if (  keys %>% grepl("mm", ., ignore.case = TRUE) %>% any(.)  ) {
    str_bounds[1] <- min(6, str_bounds[1])
    str_bounds[2] <- max(7, str_bounds[2])
    # interval_bounds <- c(min = cosdata[viscos_options("name_COSmonth")] %>% min(.),
    #                      max = cosdata[viscos_options("name_COSmonth")] %>% max(.))
  }
  if (  keys %>% grepl("yyyy", ., ignore.case = TRUE) %>% any(.)  ) {
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