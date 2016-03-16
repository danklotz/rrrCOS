#' save the the list barplots
#' 
#' saves the list returned by serve_expanded_barplots xxx more description will follow TM
#' @export
serve_expandedbars_intoFile <- function(list_of_barplts, path = "",jpg_filenames = "bar_plot", hmtl_filename = "summary_file") {
  # pre
  require(magrittr, quietly = TRUE)
  require(ggplot2, quietly = TRUE)
  require(gridExtra, quietly = TRUE)
  '%&%' <- function(a,b) paste(a,b,sep = "") # helper for easiser string concatenation
  
  # calc
  ## connect to a html file in www folder
    fileConn <- file(path %&% hmtl_filename %&% ".html" , "w")
  ## write html header
    writeLines(text = '<!DOCTYPE html>',fileConn)
    writeLines(text = '<html>',fileConn)
    writeLines(text = '<body>',fileConn)
  ## get number of necessary plots
    eval_size <- length(list_of_barplts)
    num_plots <- ceiling(eval_size/9)
  ## save everything localy & link it within the html file
    for ( i in 1:(num_plots) )
    {
      j = g[i]
      plt_name <- jpg_filenames %&% i %&% ".jpg"
      plt_pathANDname <- path %&% plt_name
      plt_hmtlInfos <- "<img src=\"" %&% plt_name %&% '" alt="nothing" style="width:800px;height:500px;">'
      #
      writeLines(text = plt_hmtlInfos,fileConn )
      jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
      from <- 1 + (i-1)*9
      to <- min( eval_size , from + 9 )
      do.call("grid.arrange",c(list_of_barplts[from:to],list(ncol = 3, nrow = 3) ))
      dev.off()
    }
    #§ note necessary anymore !!??
#     if (num_plots > 1) {
#       plt_name <- jpg_filenames %&% i+1 %&% ".jpg"
#       plt_pathANDname <- path %&% plt_name
#       plt_hmtlInfos <- "<img src=\"" %&% plt_name %&% '" alt="nothing" style="width:800px;height:500px;">'
#       #
#       writeLines( plt_hmtlInfos,fileConn)
#       jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
#       do.call("grid.arrange",c(list_of_barplts[(j+9):eval_size],list(ncol = 3, nrow = 3) ))
#       dev.off()
#       close(fileConn)
#     }
    close(fileConn)
}