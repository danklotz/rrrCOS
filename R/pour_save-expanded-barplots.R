pour.save_expbars <- function(list_barplts,eval_size,s_ctrl) {
  # some pre sets for calculations:
  num_plots <- ceiling(eval_size/9)
  g <- seq(from = 1, to = (eval_size-9) , by=9)
  max_plots <- (length(g)+1)
  # connect to a html file in www folder
  fileConn <- file(paste("R/App/www/",s_ctrl$hmtlfilename,".html",sep=""),"w")
  # write html header
  writeLines(text = '<!DOCTYPE html>',fileConn)
  writeLines(text = '<html>',fileConn)
  writeLines(text = '<body>',fileConn)
  # save everything localy & link it within the html file
  for ( i in 1:(max_plots-1) )
  {
    j = g[i]
    plt_name <- paste(s_ctrl$jpgfilename ,i,".jpg", sep = "")
    plt_pathANDname <- paste("R/App/www/",plt_name,sep = "")
    plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
    #
    writeLines(text = plt_hmtlInfos,fileConn )
    jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
    do.call("grid.arrange",c(list_barplts[j:(j+8)],list(ncol = 3, nrow = 3) ))
    dev.off()
  }
  plt_name <- paste(s_ctrl$jpgfilename ,i+1,".jpg", sep="")
  plt_pathANDname <- paste("R/App/www/",plt_name,sep="")
  plt_hmtlInfos <- paste("<img src=\"",plt_name,'" alt="nothing" style="width:800px;height:500px;">' ,sep = "")
  #
  writeLines( plt_hmtlInfos,fileConn)
  jpeg(file = plt_pathANDname, width = 800, height = 500, units = "px")
  do.call("grid.arrange",c(list_barplts[(j+9):eval_size],list(ncol = 3, nrow = 3) ))
  dev.off()
  close(fileConn)
}