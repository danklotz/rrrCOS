pour.expandedbars.intoPath <- function(list_barplts,eval_size,name_of_pictures = bar_plot, name_of_htmlfile = summary_file) {
  # pre
  require(magrittr)
  '%&%' <- function(a,b) paste(a,b,sep = "") # helper for easiser string concatenation
  
  # calc
  #
  num_plots <- ceiling(eval_size/9)
  g <- seq(from = 1, to = (eval_size-9) , by=9)
  max_plots <- (length(g)+1)
  # connect to a html file in www folder
  fileConn <- file("R/App/www/" %&% name_of_htmlfile %&% ".html" , "w")
  # write html header
  writeLines(text = '<!DOCTYPE html>',fileConn)
  writeLines(text = '<html>',fileConn)
  writeLines(text = '<body>',fileConn)
  # save everything localy & link it within the html file
  for ( i in 1:(max_plots-1) )
  {
    j = g[i]
    plt_name <- name_of_pictures %&% i %&% ".jpg"
    plt_pathANDname <- "R/App/www/" %&% plt_name
    plt_hmtlInfos <- "<img src=\"" %&% plt_name %&% '" alt="nothing" style="width:800px;height:500px;">'
    #
    writeLines(text = plt_hmtlInfos,fileConn )
    jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
    do.call("grid.arrange",c(list_barplts[j:(j+8)],list(ncol = 3, nrow = 3) ))
    dev.off()
  }
  plt_name <- name_of_pictures %&% i+1 %&% ".jpg"
  plt_pathANDname <- "R/App/www/" %&% plt_name
  plt_hmtlInfos <- "<img src=\"" %&% plt_name %&% '" alt="nothing" style="width:800px;height:500px;">'
  #
  writeLines( plt_hmtlInfos,fileConn)
  jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
  do.call("grid.arrange",c(list_barplts[(j+9):eval_size],list(ncol = 3, nrow = 3) ))
  dev.off()
  close(fileConn)
}