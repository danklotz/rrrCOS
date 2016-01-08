visCOS.example <- function(runoff_path,spinup,ctrl) {
  # loaded dependencies 
#    require("data.table") 
#    require("shiny")
#    require("hydroGOF")
#    require("dygraphs")
#    require("ggplot2")
#    require("xts")
#    require("dplyr")
#    require("grid")
#    require("gridExtra")
#    require("reshape2")
  #
#   source("R/f_expanded_barplots.R")
#   source("R/f_rasterplot_functions.R")
  ######################################################################################
  # SETUP #§ temporary !?
    ctrl <- fetch.ctrl()
    ctrl$pathDotRunoff  <- file.choose()
  # load runoff files
    require("dplyr")
    require("data.table")
    d_raw <- fread(ctrl$pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
        as.data.frame(.)
  # eliminate basins withouth observations:
    d_runoff <- d_raw %>% 
      channel.removeChunk %>% 
      channel.onlyObserved
    
  # get num of used basins and their respective num
    #§ shall I wrap this into a channel function??
    d_nums <- fetch.d_num(d_runoff)
  # remove spinup-time
    path_Spinup <- channel.path(ctrl$pathDotRunoff) %>% paste("Statistics.txt", sep="") 
    pattern_spinup <- "start time-step of evaluation"
    spinup <- fetch.spinup(path_Spinup,pattern_spinup)
    #
    d_runoff <- slice( d_runoff,spinup:dim(d_runoff)[1] )
  # add full date information to data 
    d_runoff$POSIXdate <- implode.Cosdate(d_runoff)
  # convert d_runoff to time series object (i.e. "xts")
    d_xts <- channel.dxts(d_runoff)
  # calculate hydrological years:
    d_runoff <- channel.hydyears(d_runoff)
    years <- fetch.yearsindata(d_runoff)
    #§ its not realy smart to handle it like this, whit two strange variables. Maybe better solution possible?
    hydyears_in_d <- fetch.hydyears(d_runoff,years)
    num_hydyears <- length(hydyears_in_d)
    
######################################################################################
    
  # do calculations
  # calculations:
    require(hydroGOF)
    bOF <- fetch.basicOfun(d_runoff,hydyears_in_d)
  #$ write out NSE .txt & total text files
  #§ not sure if this should be a pour function?? maybe the user should to it??? 
#     pathtoOut <- "R/App/www/" #§ temporary solution
#     write.table(cbind(d_nums,t(NSE_hydyearly), NSE_total),
#               file = paste(pathtoOut,"NSE_Hydyear.csv", sep = ""),
#               row.names = FALSE, col.names = c("#",paste("HY",hydyears_in_d),"TOTAL"), quote = FALSE, sep = ";")
#     cbind(d_nums,NSE_total,KGE_total,pBIAS_total,cor_total) %>%
#     write.table(.,file = paste(pathtoOut,"OF_total.csv", sep = ""),
#                 row.names = FALSE, 
#                 col.names = c("#","NSEtotal","KGEtotal","pBIAStotal","CORRtotal"),
#                 sep = ";")
#     rm(pathtoOut)
  #
  
  ######################################################################################
  # plots: NSE
  ######################################################################################
  #********************************
  # yearly
  #********************************
  plt_ctrl <- list() # reset list 
  plt_ctrl$gtitle <- "Yearly NSE"
  plt_ctrl$ylab <- "basin number"
  plt_ctrl$xlab <- ctrl$yearName
  plt_ctrl$clr1 <- ctrl$colors[1]
  plt_ctrl$clr2 <- ctrl$colors[2]
  plt_ctrl$clr3 <- ctrl$colors[3]
  plt_ctrl$clr4 <- ctrl$colors[4]
  plt_ctrl$midpoint <- ctrl$clr_NSEmid
  plt_ctrl$limits <- c(0,1)
  plt_ctrl$lb_cut <- 0.0
  #
  plt_ynse <- plt_yOF(NSE_hydyearly,hydyears_in_d,eval_size,plt_ctrl)
  #********************************
  # total
  #********************************
  plt_ctrl$gtitle <- "Total NSE   "
  plt_ctrl$ltitle <- "NSE"
  #
  plt_tnse <- plt_tOF(NSE_total,eval_size, plt_ctrl)
  
  
  #********************************
  # expanded barplots & htmlfiles
  #********************************
  # update list
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "NSE"
  #
  plt_exp_NSE <- list_yOF_barplts(NSE_hydyearly,eval_size,d_nums,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  s_ctrl$hmtlfilename <- "expnd_nse"
  s_ctrl$jpgfilename <- "expnd_nse"
  save_expnd_barplts(plt_exp_NSE,eval_size,s_ctrl)
  
  ######################################################################################
  # plots: %-bias
  ######################################################################################
  #********************************
  # yearly
  #********************************
  plt_ctrl <- list() # reset list 
  plt_ctrl$gtitle <- "Yearly %-Bias"
  plt_ctrl$ylab <- "basin number"
  plt_ctrl$xlab <- ctrl$yearName
  plt_ctrl$clr1 <- ctrl$colors[4]
  plt_ctrl$clr2 <- ctrl$colors[3]
  plt_ctrl$clr3 <- ctrl$colors[1]
  plt_ctrl$clr4 <- ctrl$colors[1]
  plt_ctrl$midpoint <- 0.0
  plt_ctrl$limits <- c(-100,100)
  plt_ctrl$lb_cut <- -1000.0
  #
  plt_ypbias <- plt_yOF(pBias_hydyearly,hydyears_in_d,eval_size,plt_ctrl)
  
  #********************************
  # total
  #********************************
  plt_ctrl$gtitle <- "Total %-Bias"
  plt_ctrl$ltitle <- "%-Bias"
  
  #
  plt_tpbias <- plt_tOF(pBIAS_total,eval_size, plt_ctrl)
  
  #********************************
  # expanded barplots & htmlfiles
  #********************************
  # pre-sets & plotting list creation
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "%-Bias"
  #
  plt_exp_pBias <- list_yOF_barplts(pBias_hydyearly,eval_size,d_nums,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  s_ctrl$hmtlfilename <- "expnd_pbias"
  s_ctrl$jpgfilename <- "expnd_pbias"
  save_expnd_barplts(plt_exp_pBias,eval_size,s_ctrl)
  
  ######################################################################################
  # plots: KGE 
  ######################################################################################
  #********************************
  # yearly 
  #********************************
  plt_ctrl <- list() # reset list 
  plt_ctrl$gtitle <- "Yearly KGE"
  plt_ctrl$ylab <- "basin number"
  plt_ctrl$xlab <- ctrl$yearName
  plt_ctrl$clr1 <- ctrl$colors[1]
  plt_ctrl$clr2 <- ctrl$colors[2]
  plt_ctrl$clr3 <- ctrl$colors[3]
  plt_ctrl$clr4 <- ctrl$colors[4]
  plt_ctrl$midpoint <- ctrl$clr_NSEmid
  plt_ctrl$limits <- c(0,1)
  plt_ctrl$lb_cut <- 0.0
  #
  plt_ykge <- plt_yOF(KGE_hydyearly,hydyears_in_d,eval_size,plt_ctrl)
  #********************************
  # total
  #********************************
  plt_ctrl$gtitle <- "Total KGE   "
  plt_ctrl$ltitle <- "KGE"
  
  #
  plt_tkge <- plt_tOF(KGE_total,eval_size, plt_ctrl)
  #********************************
  # expanded barplots & htmlfiles
  #********************************
  # update list
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "KGE"
  #
  plt_exp_KGE <- list_yOF_barplts(KGE_hydyearly,eval_size,d_nums,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  s_ctrl$hmtlfilename <- "expnd_kge"
  s_ctrl$jpgfilename <- "expnd_kge"
  save_expnd_barplts(plt_exp_KGE,eval_size,s_ctrl)
  
  ######################################################################################
  # plots: Correlation
  ######################################################################################
  #********************************
  # yearly 
  #********************************
  plt_ctrl <- list() # reset list 
  plt_ctrl$gtitle <- "Yearly Correlation"
  plt_ctrl$ylab <- "basin number"
  plt_ctrl$xlab <- ctrl$yearName
  plt_ctrl$clr1 <- ctrl$colors[1]
  plt_ctrl$clr2 <- ctrl$colors[2]
  plt_ctrl$clr3 <- ctrl$colors[3]
  plt_ctrl$clr4 <- ctrl$colors[4]
  plt_ctrl$midpoint <- ctrl$clr_NSEmid
  plt_ctrl$limits <- c(0,1)
  plt_ctrl$lb_cut <- -10.
  #
  plt_ycor <- plt_yOF(cor_hydyearly,hydyears_in_d,eval_size,plt_ctrl)
  
  #********************************
  # total
  #********************************
  plt_ctrl$gtitle <- "Total Corr  "
  plt_ctrl$ltitle <- "Corr"
  
  #
  plt_tcor <- plt_tOF(cor_total,eval_size, plt_ctrl)
  
  #********************************
  # expanded barplots & htmlfiles
  #********************************
  # update list
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "Correlation"
  #
  plt_exp_cor <- list_yOF_barplts(cor_hydyearly,eval_size,d_nums,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  s_ctrl$hmtlfilename <- "expnd_cor"
  s_ctrl$jpgfilename <- "expnd_cor"
  save_expnd_barplts(plt_exp_cor,eval_size,s_ctrl)
  
  ######################################################################################
  # now I need to run the app somehow!!!
  ######################################################################################
}
