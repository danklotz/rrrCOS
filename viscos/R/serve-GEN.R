  # --------------------------------------------------------------------------
  #' Serve is still beta
  #'
  #' More description shall follow
  #' 
  #' @keywords internal
  #' 
  #' @import pasta
  serve <- function(plotlist, path = "", fig_width = 800L, fig_height = 500L) {
    hmtl_filename <- "summary"
    # establish html-file in chosen folder
    fileConn <- file(path %&% hmtl_filename %&% ".html" , "w")
    # write html header
    #writeLines(text = '<!DOCTYPE html>',fileConn)
    writeLines(text = "<HEAD>",fileConn)
    writeLines(text = "  <STYLE type='text/css'>",fileConn)
    writeLines(text = "    H1 { text-align: center}",fileConn)
    writeLines(text = "  </STYLE>",fileConn)
   writeLines(text = "</HEAD>",fileConn)
    #writeLines(text = '<html>',fileConn)
    writeLines(text = '<body>',fileConn)
    # check which kind of plotlsit we are dealing with:
    if ( all(names(plotlist) == c("NSE","KGE","p_bias","CORR")) ) {
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
      writeLines(text = "<H1>" %&% figure_names[i] %&% "</H1>",fileConn)
      plt_name <- jpg_filenames %&% i %&% ".jpg"
      plt_pathANDname <- path %&% plt_name
      plt_hmtlInfos <- "<img src=\"" %&% 
        plt_name %&% 
        '" alt="plotting_failed" style="width:800px;height:500px;">'
      #
      writeLines(text = "<H1>" %&% plt_hmtlInfos  %&% "</H1>", fileConn)
      jpeg(file = plt_pathANDname, width = fig_width, height = fig_height, units = "px")
        plot(list_to_plot[[i]])
      dev.off()
    }
    close(fileConn)
  }
