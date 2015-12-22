list_yOF_barplts <- function(OF_hydyearly,eval_size,d_nums,d_OFyearly,plt_ctrl) {
  # creates a list of bar-plots for a the yearly objective functions (OF)
  # see: description-follows-soonTM
  # ***
  # calculations:
  # replace values under lower boundary:
  temp <- OF_hydyearly;
  temp[temp < plt_ctrl$lb_cut] <- plt_ctrl$lb_cut 
  # prepare data for plotting
  d_OFyearly <- as.data.frame(temp)
  newNames <- paste(plt_ctrl$gtitle,d_nums,sep="") 
  names(d_OFyearly)  <- newNames
  years_in_data_shrt <- as.character(years_in_data) %>% substring(.,3,4)
  d_OFyearly$hydyear <- hydyears_in_d
  # make list of plots
  barplts <- list()
  barplts <- lapply(1:eval_size, 
                    function(k) ggplot(data = d_OFyearly,environmnet = environment()) +
                      geom_bar(stat="identity",
                               position = "identity",
                               aes_string(x="hydyear", y=newNames[k],fill = newNames[k])) +
                      theme_bw(base_size = 15) +
                      ggtitle(newNames[k]) +
                      xlab(plt_ctrl$xlab) +
                      ylab(plt_ctrl$ylab ) +
                      scale_y_continuous(limits = plt_ctrl$limits ) + 
                      theme(legend.position= "none", 
                            axis.text.x = element_text(angle = 50, hjust = 1), 
                            plot.margin = grid::unit(c(0.2,0.5,0.2,0.5), "cm") ) + # von obem im urzeiger sinn
                      scale_fill_gradient2(space = "Lab", 
                                           low = plt_ctrl$clr1,
                                           mid = plt_ctrl$clr2, 
                                           high = plt_ctrl$clr3, 
                                           midpoint = plt_ctrl$midpoint, 
                                           limits = plt_ctrl$limits )
  )
  return(barplts)
} 

save_expnd_barplts <- function(list_barplts,eval_size,s_ctrl) {
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
