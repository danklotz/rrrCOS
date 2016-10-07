serve <- function(plotlist,path = "", fig_width = 800L, fig_height = 500L) {
  hmtl_filename <- "summary"
  # establish html-file in chosen folder
  '%&%' <- function(a,b) paste(a,b,sep = "") # helper for easiser string concatenation
  fileConn <- file(path %&% hmtl_filename %&% ".html" , "w")
  # write html header
  writeLines(text = '<!DOCTYPE html>',fileConn)
  writeLines(text = '<html>',fileConn)
  writeLines(text = '<body>',fileConn)
  # check which kind of plotlsit we are dealing with: 
  if ( all(names(plotlist) == c("NSE","KGE","pBIAS","CORR")) ) {
    list_to_plot <- plotlist

  } else if ( all(grepl("basin",names(plotlist1))) ) {
    list_to_plot <- unlist(plotlist,recursive = FALSE)
  } else {
    stop("plotlist not known!")
  }
  num_plots <- length(list_to_plot)
  figure_names <- names(list_to_plot)
  ## save everything localy & link it within the html file
  jpg_filenames <- "figure"
  for (i in 1:num_plots) {
    writeLines(text = figure_names[i] %&% "\\n" ,fileConn)
    plt_name <- jpg_filenames %&% i %&% ".jpg"
    plt_pathANDname <- path %&% plt_name
    plt_hmtlInfos <- "<img src=\"" %&% plt_name %&% '" alt="plotting_failed" style="width:800px;height:500px;">'
    #
    writeLines(text = plt_hmtlInfos, fileConn)
    jpeg(file = plt_pathANDname, width = fig_width, height = fig_height, units = "px")
      plot(list_to_plot[[i]])
    dev.off()
  }
  close(fileConn)
}
