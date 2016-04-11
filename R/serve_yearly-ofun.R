
# raster hydyearly NSE ----------------------------------------------------
#' NSE raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
serve.plot_period_NSE <- function(runoff_data, plt_ctrl) {
    if ( !exists("plt_ctrl") ) {
      plt_ctrl <- pour.plt_ctrl()
      plt_ctrl$plot_title <- "Yearly NSE"
      plt_ctrl$ylab <- "basin number"
    }
  # calc
  bOF = serve.period_ofun(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- serve.plot_ofun("NSE", bOF, periods_in_data, plt_ctrl)
  return(plt)
}

# raster hydyearly KGE ----------------------------------------------------
#' KGE raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
serve.plot_period_KGE<- function(runoff_data, plt_ctrl) {
    if ( !exists("plt_ctrl") ) {
      plt_ctrl <- pour.plt_ctrl()
      plt_ctrl$plot_title <- "Yearly KGE"
      plt_ctrl$ylab <- "basin number"
    }
  # calc
  bOF = serve.period_ofun(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- serve.plot_ofun("KGE", bOF, periods_in_data, plt_ctrl)
  return(plt)
}

# raster hydyearly pBIAS ----------------------------------------------------
#' Percentage Bias raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
serve.plot_period_pBIAS <- function(runoff_data, plt_ctrl) {
  if ( !exists("plt_ctrl") ) {
    plt_ctrl <- pour.plt_ctrl()
    plt_ctrl$plot_title <- "Yearly %-Bias"
    plt_ctrl$ylab <- "basin number"
    plt_ctrl$midpoint <- 0.0
    plt_ctrl$limits <- c(-100,100)
    plt_ctrl$lb_cut <- -1000.0
  }
  # calc
  bOF = serve.period_ofun(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- serve.plot_ofun("pBIAS", bOF, periods_in_data, plt_ctrl)
  return(plt)
}

# raster hydyearly Corr -----------------------------------------------------
#' Correlation raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
serve.plot_period_CORR <- function(runoff_data, plt_ctrl) {
    if ( !exists("plt_ctrl") ) {
      plt_ctrl <- pour.plt_ctrl()
      plt_ctrl$plot_title <- "Yearly Correlation"
      plt_ctrl$ylab <- "basin number"
      plt_ctrl$limits <- c(0,1)
      plt_ctrl$lb_cut <- -10.
  
    }
  # calc
  bOF = serve.period_ofun(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- serve.plot_ofun("CORR", bOF, periods_in_data, plt_ctrl)
  return(plt)
}

# serve yearly ofun ---------------------------------------------------------------
#' ggplot wrapper for the hydyearly objective functions
#'
#' plot table of the yearly basic objective function
#'
#' @param bOF list, as returned by \code{\link[visCOS]{pour.basicOfun}}
#' @param string with the chosen baisc objective function.
#' \code{\link[visCOS]{pour.basicOfun}} provides "NSE", "KGE", "pBIAS" or "CORR"
#' @param periods_in_data periods in data, as returned by \code{\link[visCOS]{pour.periods}}
#' @param xxx yet to be defined control list
#' @export
serve.plot_ofun <- function(choice,bOF,periods_in_data,plt_ctrl) {
  # def
    require(ggplot2)
    require(magrittr)
    require(reshape2)
    assert_basicOF(bOF)
    if (missing(plt_ctrl)) {
      plt_ctrl <- pour.plt_ctrl()
    }
  # calc
  if (choice == "NSE") {
    Ofun_hydyearly = bOF$NSE_periods
  } else if (choice == "KGE") {
    Ofun_hydyearly = bOF$KGE_periods
  } else if (choice == "pBIAS"){
    Ofun_hydyearly = bOF$pBIAS_periods
  } else if (choice == "CORR") {
    Ofun_hydyearly = bOF$CORR_periods
  }
  #
  eval_size <- dim(Ofun_hydyearly)[2] 
  of_y <- expand.grid(period = periods_in_data, numberBasins = 1:eval_size)
  temp <- Ofun_hydyearly
  temp[Ofun_hydyearly < plt_ctrl$lb_cut] <- plt_ctrl$lb_cut
  temp <- melt(temp)[3]
  of_y$OFvalue = round(temp$value,2)
  number_of_basins <- names(bOF$NSE) %>% gsub("\\D","",.) %>% as.integer
  # plot with ggplot2
  plt_out <- ggplot(of_y, aes(period,numberBasins, fill = OFvalue),environmnet = environment()) +
    ggtitle(plt_ctrl$plot_title) +
    geom_raster(position = "identity") +
    ylab(plt_ctrl$ylab) +
    xlab(plt_ctrl$xlab) +
    scale_y_reverse(breaks = 1:eval_size, labels = number_of_basins) +
    scale_fill_gradient2(space = "Lab",
                         low = plt_ctrl$clr1 , mid= plt_ctrl$clr2 , high = plt_ctrl$clr3 ,
                         midpoint = plt_ctrl$midpoint,
                         limits= plt_ctrl$limits,
                         na.value = plt_ctrl$clr4) +
    theme_bw(base_size = 15) +
    theme( legend.position="none" )  +
    geom_tile(color = "white", size = 0.25 ) +
    geom_text(aes(period,numberBasins, label = as.character(OFvalue)), color= "black")
  return(plt_out)
}