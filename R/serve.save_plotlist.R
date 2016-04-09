#' save a list of barplots
#' 
#' saves the list of plots as .jpeg and embeds them into an .html file
#' @export
serve.save_plotlist <- function(plot_list, 
                               path = "",
                               jpg_filenames = "plot",
                               hmtl_filename = "summary_file") {
  # def
    require("magrittr", quietly = TRUE)
    require("ggplot2", quietly = TRUE)
    require("gridExtra", quietly = TRUE)
    '%&%' <- function(a,b) paste(a,b,sep = "") # helper for easiser string concatenation
  # calc --------------------------------------------------------------------
  ## connect to a html file in www folder
  fileConn <- file(path %&% hmtl_filename %&% ".html" , "w")
  ## write html header
  writeLines(text = '<!DOCTYPE html>',fileConn)
  writeLines(text = '<html>',fileConn)
  writeLines(text = '<body>',fileConn)
  ## get number of necessary plots
  num_plots <- length(plot_list)
  ## save everything localy & link it within the html file
  for ( i in 1:num_plots )
  {
    plt_name <- jpg_filenames %&% i %&% ".jpg"
    plt_pathANDname <- path %&% plt_name
    plt_hmtlInfos <- "<img src=\"" %&% plt_name %&% '" alt="nothing" style="width:800px;height:500px;">'
    #
    writeLines(text = plt_hmtlInfos,fileConn )
    jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
      plot( plot_list[[i]] )
    dev.off()
  }
  close(fileConn)
}






