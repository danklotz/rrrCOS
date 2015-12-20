#' sweeps the filename away
#' 
#' Removes the filename from a path 
#' @param filepath path to a given file
#' @return path to the file
#' @importFrom magrittr %>%
#' @export
sweep.path <- function(filepath) {
  if (exists("filepath")) {
    sweep.path<- filepath %>%
      strsplit("\\\\") %>% .[[1]] %>% 
      strsplit("/") %>% .[[1]] %>% 
      .[1:(length(.)-1)] %>% 
      paste(.,collapse = "/") %>%
      paste(.,"/",sep="")
    return(sweep.path)
  } else { 
    stop("no filepath provided!")
#       sweep.path  <- file.choose() %>%
#         strsplit("\\\\") %>% .[[1]] %>% 
#         strsplit("/") %>% .[[1]] %>% 
#         .[1:(length(.)-1)] %>% 
#         paste(.,collapse = "/")
#       return(sweep.path)
    }
  }
