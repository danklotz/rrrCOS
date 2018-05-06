# Render html files -------------------------------------------------------

# old method ==============================================================
# library(rmarkdown)
# setwd("vignettes")
# Rmd_files <- list.files(".",pattern = "*.Rmd")
# for (name in Rmd_files){
#   render(name)
# }

# new method ==============================================================
  library(bookdown)
  # be sure to change change it to where u want to have the book :)
  out_dir <- "/Users/dan/Documents/danklotz.github.io/viscos/"
  setwd("vignettes")
  #out_dir <- "D:/danklotz.github.io/viscos" # note: do not end path with /
  # generate pdf file 
  render_book(input = "index.Rmd", 
              output_format = "bookdown::pdf_book", 
              output_dir = out_dir)
  # generate html book
  render_book(input = "index.Rmd", 
              output_format = "bookdown::gitbook", 
              output_dir = out_dir)

  