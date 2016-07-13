#' hydyearly list of expaned barpltos 
#'
#'
#' creates a list of bar-plots for a the yearly objective functions (OF) xxx
#' see: description-follows-soonTM xxx
#' @export
serve.plotlist_periodOF <- function(runoff_data,bOF) {
    require("ggplot2", quietly = TRUE)
    if (missing(bOF)) {bOF <- extract_objective_functions(runoff_data)}
  # calc: -------------------------------------------------------------------
  ofun_hydyearly <- bOF$NSE_period
  num_basins <- get_basin_numbers(runoff_data)
  # 
  rename.withEndings <- function(dataframe,name,endings) {
    new_names <- paste(name,endings,sep = "")
    names(dataframe)  <- new_names
    return(dataframe)
  }
  d_ofun_yearly <- ofun_hydyearly %>% 
                    pmax(.,viscos_options()$lb_cut) %>%
                    as.data.frame %>%
                    rename.withEndings(.,viscos_options()$plot_title,num_basins)
  new_names <- names(d_ofun_yearly)
  d_ofun_yearly$hydyear <- unique(runoff_data$period) %>% extract(.>0)
  # make list of plots
  list_of_barplots <- list()
  eval_size <- ncol(ofun_hydyearly)
  # define plot function: 
  plot_function <- function(k) {
    ggplot(data = d_ofun_yearly,environmnet = environment()) +
    geom_bar(stat = "identity",
             position = "identity",
             aes_string(x = "hydyear", y = new_names[k],fill = new_names[k])) +
    theme_bw(base_size = 15) +
    ggtitle(new_names[k]) +
    xlab(viscos_options()$xlab) +
    ylab( "NSE" ) +
    scale_y_continuous(limits = viscos_options()$limits ) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 50, hjust = 1),
          plot.margin = grid::unit(c(0.2,0.5,0.2,0.5), "cm") ) + # clockwise form above
    scale_fill_gradient2(space = "Lab",
                         low = viscos_options()$clr1,
                         mid = viscos_options()$clr2,
                         high = viscos_options()$clr3,
                         midpoint = viscos_options()$midpoint,
                         limits = viscos_options()$limits )
  }
  # apply plot function over all available stuff
  list_of_barplots <- lapply(1:eval_size,
                    function(k) plot_function(k)
  )
  return(list_of_barplots)
}