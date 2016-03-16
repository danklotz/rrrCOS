#' Removes the filename from a path 
#' 
#' Removes the filename from a given path 
#' @param filepath path to a given file
#' @return path to the file
#' @export
prepare_path <- function(filepath) {
  ##########################
  # pre
  ##########################
  require(magrittr)
  ##########################
  # calc
  ##########################
  if (exists("filepath")) {
    # check for system depented path 
    sysIswindos <- grepl('\\\\',filepath)
    if (sysIswindos) {
      prepared_path <- filepath %>%
        strsplit("\\\\") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(prepared_path)
    } else { 
      prepared_path <- filepath %>%
        strsplit("/") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(prepared_path)
    }
  } else { 
    stop("no filepath provided!")
    #       prepared_path  <- file.choose() %>%
    #         strsplit("\\\\") %>% .[[1]] %>% 
    #         strsplit("/") %>% .[[1]] %>% 
    #         .[1:(length(.)-1)] %>% 
    #         paste(.,collapse = "/")
    #       return(prepared_path)
  }
}