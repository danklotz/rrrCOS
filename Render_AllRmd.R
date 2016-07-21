# Render all html files
require(rmarkdown)
setwd("vignettes")
Rmd_files <- list.files(".",pattern = "*.Rmd")
for (name in Rmd_files){
  render(name)
}
