#' Removes the filename from a path 
#' 
#' Removes the filename from a given path 
#' @param filepath path to a given file
#' @return path to the file
#' @export
channel.path <- function(filepath) {
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
      channel.path <- filepath %>%
        strsplit("\\\\") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(channel.path)
    } else { 
      channel.path <- filepath %>%
        strsplit("/") %>% .[[1]] %>% 
        .[1:(length(.)-1)] %>% 
        paste(.,collapse = "/") %>%
        paste(.,"/",sep="")
      return(channel.path)
    }
  } else { 
    stop("no filepath provided!")
    #       channel.path  <- file.choose() %>%
    #         strsplit("\\\\") %>% .[[1]] %>% 
    #         strsplit("/") %>% .[[1]] %>% 
    #         .[1:(length(.)-1)] %>% 
    #         paste(.,collapse = "/")
    #       return(channel.path)
  }
}