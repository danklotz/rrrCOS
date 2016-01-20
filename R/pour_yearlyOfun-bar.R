#' yearly runoff list
#'
#'
#' creates a list of bar-plots for a the yearly objective functions (OF)
#' see: description-follows-soonTM
#' ***
# replace values under lower boundary:
pour.yearlyOfun_bar <- function(Ofun_hydyearly,eval_size,d_nums,plt_ctrl) {
  # calculations:
  temp <- Ofun_hydyearly;
  temp[temp < plt_ctrl$lb_cut] <- plt_ctrl$lb_cut
  # prepare data for plotting
  d_OFyearly <- as.data.frame(temp)
  newNames <- paste(plt_ctrl$gtitle,d_nums,sep = "")
  names(d_OFyearly)  <- newNames
  d_OFyearly$hydyear <- hydyears_in_d
  # make list of plots
  barplts <- list()
  barplts <- lapply(1:eval_size,
                    function(k) ggplot(data = d_OFyearly,environmnet = environment()) +
                      geom_bar(stat="identity",
                               position = "identity",
                               aes_string(x= "hydyear", y = newNames[k],fill = newNames[k])) +
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