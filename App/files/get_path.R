loldir <- file.choose() %>% strsplit("/") %>% .[[1]]%>% .[1:(length(.)-1)] %>% paste(.,collapse = "/")
setwd(loldir)
