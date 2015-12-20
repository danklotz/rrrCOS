  get.runoffpath <- function() {
    get.runoffpath  <- file.choose() %>% 
      strsplit("\\\\") %>% .[[1]] %>% 
      strsplit("/") %>% .[[1]] %>% 
      .[1:(length(.)-1)] %>% 
      paste(.,collapse = "/")
    return(get.runoffpath)
  }
