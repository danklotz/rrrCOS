# Render html files -------------------------------------------------------

# old method ==============================================================
# require(rmarkdown)
# setwd("vignettes")
# Rmd_files <- list.files(".",pattern = "*.Rmd")
# for (name in Rmd_files){
#   render(name)
# }

# new method ==============================================================
  require(bookdown)
  setwd("vignettes")
  # generate pdf file 
  render_book("index.Rmd", "bookdown::pdf_book")
  # generate html book
  render_book("index.Rmd", "bookdown::gitbook")
