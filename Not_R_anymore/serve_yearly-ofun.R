
# raster hydyearly NSE ----------------------------------------------------
#' NSE raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
periodplot_NSE <- function(runoff_data, plot_title = "yearly NSE", y_label = "basin number") {
  plt_ctrl <- viscos_options()
  plt_ctrl$plot_title <- plot_title
  plt_ctrl$ylab <- y_label
  # calc
  bOF <- extract_objective_functions(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- periodplot_ofun("NSE", bOF, periods_in_data, plt_ctrl)
  return(plt)
}

# raster hydyearly KGE ----------------------------------------------------
#' KGE raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
periodplot_KGE<- function(runoff_data, plot_title = "Yearly KGE", y_label = "basin number") {
  plt_ctrl <- viscos_options()
  plt_ctrl$plot_title <- plot_title
  plt_ctrl$ylab <- y_label
  # calc
  bOF <- extract_of(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- periodplot_ofun("KGE", bOF, periods_in_data, plt_ctrl)
  return(plt)
}

# raster hydyearly pBIAS ----------------------------------------------------
#' Percentage Bias raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
periodplot_pBIAS <- function(runoff_data, plt_ctrl) {
  plt_ctrl <- viscos_options()
  plt_ctrl$plot_title <- "Yearly %-Bias"
  plt_ctrl$ylab <- "basin number"
  # calc
  bOF <- extract_of(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- periodplot_ofun("pBIAS", bOF, periods_in_data, plt_ctrl)
  return(plt)
}

# raster hydyearly Corr -----------------------------------------------------
#' Correlation raster plot for the available hydrological years
#' xxx description to follow 
#' 
#' @export
periodplot_CORR <- function(runoff_data, plt_ctrl) {
    if ( !exists("plt_ctrl") ) {
      plt_ctrl <- pour.plt_ctrl()
      plt_ctrl$plot_title <- "Yearly Correlation"
      plt_ctrl$ylab <- "basin number"
      plt_ctrl$limits <- c(0,1)
      plt_ctrl$lb_cut <- -10.
  
    }
  # calc
  bOF <- extract_of(runoff_data)
  periods_in_data <- unique(runoff_data$period)
  plt <- periodplot_ofun("CORR", bOF, periods_in_data, plt_ctrl)
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
periodplot_ofun <- function(choice,bOF,periods_in_data,plt_ctrl) {
  # def
    require(ggplot2, quietly = TRUE)
    require(magrittr, quietly = TRUE)
    require(reshape2, quietly = TRUE)
  # calc
  if (choice == "NSE") {
    Ofun_hydyearly = bOF[grep("NSE_period.*",bOF$of),]
  } else if (choice == "KGE") {
    Ofun_hydyearly = bOF$KGE_periods
  } else if (choice == "pBIAS"){
    Ofun_hydyearly = bOF$pBIAS_periods
  } else if (choice == "CORR") {
    Ofun_hydyearly = bOF$CORR_periods
  }
  #
  eval_size <- ncol(Ofun_hydyearly) - 1
  periods_in_data <- periods_in_data[periods_in_data > 0]
  of_y <- expand.grid(period = periods_in_data, numberBasins = 1:eval_size)
  temp <- Ofun_hydyearly[ ,2:(eval_size+1)]
  temp[temp < viscos_options("lb_cut")] <- plt_ctrl$lb_cut
  temp <- melt(temp)
  of_y$OFvalue = round(temp$value,2)
  number_of_basins <- 1:eval_size #names(bOF$NSE) %>% gsub("\\D","",.) %>% as.integer
  
  of_y <- rbind(of_y,c(0,1,0.3))
  of_y <- rbind(of_y,c(0,2,0.4))
  of_y <- cbind(of_y,test = c("period","period","period","period","overall","overall"))
  
  # plot with ggplot2
  plt_out <- ggplot(of_y, aes(period,numberBasins, fill = OFvalue),environmnet = environment()) +
      geom_raster(position = "identity") +
      coord_fixed(ratio = 5) + 
      facet_grid(~test,  scales = "free_x", space = "free") + 
      scale_x_continuous(breaks = periods_in_data, labels = periods_in_data) +
      scale_y_reverse(breaks = 1:eval_size, labels = number_of_basins) +
      scale_fill_gradient2(space = "Lab",
                           low = viscos_options("color_of_low"),
                           mid= viscos_options("color_of_mid"), 
                           high = viscos_options("color_of_high"),
                           midpoint = viscos_options("midpoint"),
                           limits= viscos_options("limits"),
                           na.value = viscos_options("color_of_out")) +
      theme( legend.position="none" )  +
      geom_tile(color = "white", size = 0.25 ) +
      geom_text(aes(period,numberBasins, label = as.character(OFvalue)), color= "black") 
  return(plt_out)
}