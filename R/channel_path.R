#' Removes the filename from a path 
#' 
#' Removes the filename from a given path 
#' @param filepath path to a given file
#' @return path to the file
#' @export
channel_path <- function(filepath) {
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
      channel_path <- filepath %>%
        strsplit("\\\\") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(channel_path)
    } else { 
      channel_path <- filepath %>%
        strsplit("/") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(channel_path)
    }
  } else { 
    stop("no filepath provided!")
    #       channel_path  <- file.choose() %>%
    #         strsplit("\\\\") %>% .[[1]] %>% 
    #         strsplit("/") %>% .[[1]] %>% 
    #         .[1:(length(.)-1)] %>% 
    #         paste(.,collapse = "/")
    #       return(channel_path)
  }
}