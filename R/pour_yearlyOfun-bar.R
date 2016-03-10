#' hydyearly list of expaned barpltos 
#'
#'
#' creates a list of bar-plots for a the yearly objective functions (OF) xxx
#' see: description-follows-soonTM xxx
#' @export
pour_expanded_barplots <- function(Ofun_hydyearly,hydyears_in_data,num_basins,plt_ctrl) {
  if (missing(plt_ctrl)) {plt_ctrl <- fetch_plt_ctrl()}
  # calc:
  rename.withEndings <- function(dataframe,name,endings) {
    newNames <- paste(name,endings,sep = "")
    names(dataframe)  <- newNames
    return(dataframe)
  }
  d_ofun_yearly <- Ofun_hydyearly %>% 
                    cut.lowerbound(.,plt_ctrl$lb_cut) %>% 
                    as.data.frame %>%
                    rename.withEndings(.,plt_ctrl$gtitle,num_basins)
  d_ofun_yearly$hydyear <- hydyears_in_data
  # make list of plots
  list_of_barplots <- list()
  eval_size <- ncol(Ofun_hydyearly)
  list_of_barplots <- lapply(1:eval_size,
                    function(k) ggplot(data = d_ofun_yearly,environmnet = environment()) +
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
  return(list_of_barplots)
}