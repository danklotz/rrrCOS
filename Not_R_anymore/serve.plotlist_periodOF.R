#' hydyearly list of expaned barpltos 
#'
#'
#' creates a list of bar-plots for a the yearly objective functions (OF) xxx
#' see: description-follows-soonTM xxx
#' @export
serve.plotlist_periodOF <- function(runoff_data,plt_ctrl,bOF) {
  # ofun_hydyearly,periods,num_basins,plt_ctrl) {
    require("ggplot2", quietly = TRUE)
    if (missing(bOF)) {bOF <- extract_of(runoff_data)}
  # calc: -------------------------------------------------------------------
  ofun_hydyearly <- bOF$NSE_periods
  periods <- unique(runoff_data$period) %>% extract(.>0)
  num_basins <- get_basin_numbers(d_runoff)
  # 
  rename.withEndings <- function(dataframe,name,endings) {
    new_names <- paste(name,endings,sep = "")
    names(dataframe)  <- new_names
    return(dataframe)
  }
  d_ofun_yearly <- ofun_hydyearly %>% 
                    pmax(.,plt_ctrl$lb_cut) %>%
                    as.data.frame %>%
                    rename.withEndings(.,plt_ctrl$plot_title,num_basins)
  new_names <- names(d_ofun_yearly)
  d_ofun_yearly$hydyear <- periods
  # make list of plots
  list_of_barplots <- list()
  eval_size <- ncol(ofun_hydyearly)
  # define plot function: 
  plot_function <- function(k) {
    ggplot(data = d_ofun_yearly,environmnet = environment()) +
    geom_bar(stat="identity",
             position = "identity",
             aes_string(x= "hydyear", y = new_names[k],fill = new_names[k])) +
    theme_bw(base_size = 15) +
    ggtitle(new_names[k]) +
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
  }
  # apply plot function over all available stuff
  list_of_barplots <- lapply(1:eval_size,
                    function(k) plot_function(k)
  )
  return(list_of_barplots)
}