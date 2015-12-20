# define rasterplot funcions for yearly OF
plt_yOF <- function(OF_hydyearly,hydyears_in_d,eval_size,plt_ctrl) {
  of_y <- expand.grid(hydyears = hydyears_in_d, numberBasins = 1:eval_size) 
  temp <- OF_hydyearly; 
  temp[temp < plt_ctrl$lb_cut] <- plt_ctrl$lb_cut
  temp <- reshape2::melt(temp)[3]
  of_y$OFvalue = round(temp$value,2)
  #
  plt_out <- ggplot(of_y, aes(hydyears,numberBasins, fill = OFvalue),environmnet = environment()) + 
    ggtitle(plt_ctrl$gtitle) + 
    geom_raster(position = "identity") + 
    ylab(plt_ctrl$ylab) + 
    xlab(plt_ctrl$xlab) + 
    scale_y_reverse(breaks = 1:eval_size, labels = d_nums) + 
    scale_x_discrete( breaks = hydyears_in_d) +
    scale_fill_gradient2(space = "Lab", 
                         low = plt_ctrl$clr1 , mid= plt_ctrl$clr2 , high = plt_ctrl$clr3 ,
                         midpoint = plt_ctrl$midpoint, 
                         limits= plt_ctrl$limits ,
                         na.value = plt_ctrl$clr4) +
    theme_bw(base_size = 20) +
    theme( legend.position="none" )  + 
    geom_tile(color = "white", size = 0.25 ) + 
    geom_text(aes(hydyears,numberBasins, label = as.character(OFvalue)), size = ctrl$OFsize , color= "black")
  return(plt_out)
}

# define rasterplot functions for total OF 
plt_tOF <- function(OF_total,eval_size,plt_ctrl) {
  of_t <- expand.grid(total = 1, numberBasins = 1:eval_size) 
  temp <- OF_total; 
  # replace values under lower boundary:
  temp[temp < plt_ctrl$lb_cut] <- plt_ctrl$lb_cut 
  # prepare dataframe for ggplot
  temp <- melt(temp)
  of_t$OFvalue = temp$value
  #
  plt_t <- ggplot(of_t , aes(total,numberBasins, fill = OFvalue),environmnet = environment()) +
    geom_raster(position = "identity") +
    ggtitle(plt_ctrl$gtitle) + 
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 20),
          legend.key.width = unit(3,"line"),
          legend.key.height = unit(4,"line"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = grid::unit(c(0.5,0.5,1.25,-0.7), "cm") ) + # von oben im urzeiger sinn
    geom_tile(color="white", size = 0.25) + 
    geom_text(aes( total, numberBasins ,label = round(OFvalue,2) ), size = ctrl$OFsize ,color="black") +
    scale_y_reverse() +
    scale_fill_gradient2(space = "Lab",
                         name = plt_ctrl$ltitle,
                         low = plt_ctrl$clr1,
                         mid= plt_ctrl$clr2, 
                         high = plt_ctrl$clr3,
                         midpoint = plt_ctrl$midpoint, 
                         limits = plt_ctrl$limits,
                         na.value = plt_ctrl$clr3)
  return(plt_t)
}