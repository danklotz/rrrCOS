# pour bOFy ---------------------------------------------------------------
#' ggplot wrapper for the hydyearly objective functions
#'
#' plot table of the yearly basic objective function
#'
#' @param bOF list, as returned by \code{\link[visCOS]{fetch.basicOfun}}
#' @param string with the chosen baisc objective function.
#' \code{\link[visCOS]{fetch.basicOfun}} provides "NSE", "KGE", "pBIAS" or "CORR"
#' @param hydyears_in_d hydrears in data, as returned by \code{\link[visCOS]{fetch.hydyears}}
#' @param xxx yet to be defined control list
#' @export
pour.yearlyOfun <- function(bOF,choice="NSE",hydyears_in_d,plt_ctrl) {
  require(ggplot2)
  require(magrittr)
  require(reshape2)
  assert.basicOF(bOF)
  if (choice == "NSE") {
    Ofun_hydyearly = bOF$NSE.hydyearly
  } else if (choice == "KGE") {
    Ofun_hydyearly = bOF$KGE.hydyearly
  } else if (choice == "pBIAS"){
    Ofun_hydyearly = bOF$pBIAS.hydyearly
  } else if (choice == "CORR") {
    Ofun_hydyearly = bOF$CORR.hydyearly
  }
  #
  eval_size <- dim(Ofun_hydyearly)[2] # NsE is just arbitrary, don't worry
  of_y <- expand.grid(hydyears = hydyears_in_d, numberBasins = 1:eval_size)
  temp <- Ofun_hydyearly;
  temp[Ofun_hydyearly < plt_ctrl$lb_cut] <- plt_ctrl$lb_cut
  temp <- melt(temp)[3]
  of_y$OFvalue = round(temp$value,2)
  # plot with ggplot2
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