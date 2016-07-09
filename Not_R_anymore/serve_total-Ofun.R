
# NSE ---------------------------------------------------------------------
#' rasterpolots for the NSE of the whole period
#' 
#' xxx description follows
#' @export
totalplot_NSE <- function(runoff_data,plot_title = "Total NSE",legend_title = "NSE") {
  # def
  plt_ctrl <- viscos_options()
  plt_ctrl$plot_title <- plot_title
  plt_ctrl$legend_title <- legend_title
  # calc
  bOF <- serve.period_ofun(runoff_data)
  total <- totalplot_Ofun("NSE",bOF,plt_ctrl) 
  return(total)
}

# KGE ---------------------------------------------------------------------
#' rasterpolots for the KGE of the whole period
#' 
#' xxx description follows
#' @export
totalplot_KGE <- function(runoff_data, plot_titel = "Total KGE",legend_title = "KGE") {
  # def
  plt_ctrl <- viscos_options()
  plt_ctrl$plot_title <- plot_title
  plt_ctrl$legend_title <- legend_title
  # calc
  bOF = serve.period_ofun(runoff_data)
  total <- totalplot_Ofun("KGE",bOF,plt_ctrl) 
  return(total)
}


# define rasterplot functions for total OF
totalplot_Ofun <- function(choice,bOF,plt_ctrl) {
  # def 
  assert_of(bOF)
  require(ggplot2, quietly = TRUE)
  #
  Ofun_total<- bOF[[choice]]
  eval_size <- length(Ofun_total)
  of_t <- expand.grid(total = 1, numberBasins = 1:eval_size)
  # replace values under lower boundary & prepare dataframe for ggplot
  temp <- Ofun_total %>% as.data.frame %>% cut.lowerbound(.,plt_ctrl$lb_cut) %>% melt(id.vars = 1)
  of_t$OFvalue = temp$.
  #
  plt_t <- ggplot(of_t , aes(total,numberBasins, fill = OFvalue),environmnet = environment()) +
    geom_raster(position = "identity") +
    ggtitle(plt_ctrl$plot_title) +
    theme_bw(base_size = 15) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.key.width = unit(3,"line"),
          legend.key.height = unit(4,"line"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = grid::unit(c(1,1,1,-0.7), "cm") ) + # von oben im urzeiger sinn
    geom_tile(color="white", size = 0.25) +
    geom_text(aes( total, numberBasins ,label = round(OFvalue,2) ) ,color="black") +
    scale_y_reverse() +
    scale_fill_gradient2(space = "Lab",
                         name = plt_ctrl$legend_title,
                         low = plt_ctrl$clr1,
                         mid= plt_ctrl$clr2,
                         high = plt_ctrl$clr3,
                         midpoint = plt_ctrl$midpoint,
                         limits = plt_ctrl$limits,
                         na.value = plt_ctrl$clr3)
  return(plt_t)
}