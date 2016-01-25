#§
# DONT USE THIS
# Temporary test file, will not be used in future
# to be replaced with Examples1.Rmd
#§

visCOS.example <- function(runoff_path,spinup,ctrl) {
  require(magrittr)
  #
  source("R/f_expanded_barplots.R")
  source("R/f_rasterplot_functions.R")
  # data wrangling --------------------------------------------------------------
  # SETUP #§ temporary !?
  ctrl <- fetch.ctrl()
  ctrl$pathDotRunoff  <- file.choose()
  # load runoff files
  
  #§ assumed to be done by the user!!!
  #       require("data.table")
  #       d_raw <- fread(ctrl$pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
  #           as.data.frame(.)
  #§
  # eliminate basins withouth observations:
  d_raw <- fetch.runoff_example()
  d_runoff <- d_raw %>% 
    channel.remove_chunk %>% 
    channel.only_observed
  # get num of used basins and their respective num
  #§ shall I wrap this into a channel function??
  num_basins <- fetch.number_of_basins(d_runoff)
  # remove spinup-time
  #§ use this later in the examples:
  #  path_Spinup <- channel.path(ctrl$pathDotRunoff) %>% paste("Statistics.txt", sep="")
  #  pattern_spinup <- "start time-step of evaluation"
  #  spinup <- fetch.spinup(path_Spinup,pattern_spinup)
  #  d_runoff <- slice( d_runoff,spinup:dim(d_runoff)[1] )
  #§
  
  # add full date information to data
  d_runoff$POSIXdate <- channel.implode_cosdate(d_runoff)
  # convert d_runoff to time series object (i.e. "xts")
  d_xts <- channel.runoff_as_xts(d_runoff)
  # calculate hydrological years:
  d_runoff <- channel.hydyears(d_runoff)
  years_in_data <- fetch.years_in_data(d_runoff)
  #§ its not realy smart to handle it like this, whith two strange variables. Maybe better solution possible?
  hydyears_in_d <- fetch.hydyears(d_runoff,years_in_data)
  num_hydyears <- length(hydyears_in_d)
  

# calculations ------------------------------------------------------------
  bOF <- fetch.some_ofun_4_hydyears(d_runoff,hydyears_in_d)
  
  
  
# plotting --------------------------------------------------------------
  ## NSE
  ### yearly
    plt_ynse <- pour.yearly_NSE(from = bOF, given = hydyears_in_d)
  ### total
    pour.totalNSE(from = bOF)
  ### expanded barplots & htmlfiles
  # fetch new list:
  plt_ctrl <- fetch.plt_ctrl()
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "NSE"
  #
  plt_exp_NSE <- list_yOF_barplts(bOF$NSE.hydyearly,years,eval_size,d_nums,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  s_ctrl$hmtlfilename <- "expnd_nse"
  s_ctrl$jpgfilename <- "expnd_nse"
  save_expnd_barplts(plt_exp_NSE,eval_size,s_ctrl)
  
  # plots: %-bias -----------------------------------------------------------
  #
  pour.hydyearly_pBIAS(from = bOF, given = hydyears_in_d)
  # total
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
  # yearly
  plt_ykge <- pour.hydyearly_KGE(from = bOF, given = hydyears_in_d)
  # total
  plt_ctrl$gtitle <- "Total KGE   "
  plt_ctrl$ltitle <- "KGE"
  #
  plt_tkge <- plt_tOF(KGE_total,eval_size, plt_ctrl)
  #********************************
  # expanded barplots & htmlfiles
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
  # yearly
  pour.hydyearly_Corr(from = bOF, given = hydyears_in_d)
  # total
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




  
  # waterbilance
  # 1. total water bilance
  d_run <- fetch.runoff()
  tmp_cum <- d_run %>%
    select(starts_with("qobs"), starts_with("qsim")) %>%
    apply(.,2,cumsum) %>%
    as.data.frame
  RegExPattern <- names(tmp_cum) %>% paste(collapse = "|")
  selectionQobsAndSim <- grepl(RegExPattern,names(d_run))
  d_cum <- d_run
  d_cum[selectionQobsAndSim] <- tmp_cum
  #2. (hydyearly water bilance)

  
  }



# test --------------------------------------------------------------------

# this is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long text
