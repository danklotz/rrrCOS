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
        require("data.table")
        d_raw <- fread(ctrl$pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
             as.data.frame(.)
        names(d_raw)[5] <- "min"
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
  # normalize data names: 
  d_runoff %<>% channel.names
  
  # convert d_runoff to time series object (i.e. "xts")
  d_xts <- channel.runoff_as_xts(d_runoff)
  # calculate hydrological years:
  d_runoff <- channel.periods(d_runoff, start_month = 9, end_month = 8)
  years_in_data <- fetch.years_in_data(d_runoff)
  #§ its not realy smart to handle it like this, whith two strange variables. Maybe better solution possible?
    # hydyears_in_d <- fetch.hydyears(d_runoff,years_in_data) #§ this is / was stupid, was it not?
  periods_in_data <- which(unique(d_runoff$period) > 0)
  num_periods <- length(periods_in_data)
  

# calculations ------------------------------------------------------------
  bOF <- fetch.period_ofun(d_runoff)
  
  
  
# plotting --------------------------------------------------------------
  ## NSE
  ### yearly
    plt_ynse <- pour.period_NSE(from = bOF, given = periods_in_data)
  ### total
    pour.totalNSE(from = bOF)
  ### expanded barplots & htmlfiles
  # fetch new list:
  plt_ctrl <- fetch.plt_ctrl()
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "NSE"
  #
  plt_exp_NSE <- pour.expanded_barplots(Ofun_hydyearly,hydyears_in_data,num_basins,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  pour.expandedbars.intoFile(plt_exp_NSE, path = "",jpg_filenames = "expnd_nse", hmtl_filename = "summary_expnd_nse")

  #§ here we go ... 
  
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




# water balance -----------------------------------------------------------
  # 1. total water bilance
  require(magrittr, quietly = TRUE)
  # d_run <- fetch.runoff_example() %>% channel.remove_chunk
  ctrl <- fetch.ctrl()
  ctrl$pathDotRunoff  <- file.choose()
  require("data.table")
  d_raw <- fread(ctrl$pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
    as.data.frame(.)
  names(d_raw)[5] <- "min"
  d_run <- d_raw %>% 
    channel.remove_chunk %>% 
    channel.only_observed  
  names(d_raw)[5] <- "min"
  tmp_cum <- d_run %>%
    select(starts_with("qobs"), starts_with("qsim")) %>%
    apply(.,2,cumsum) %>%
    as.data.frame
  selectionQobsAndSim <-  grepl( "qobs.*|qsim.*",(names(d_run)%>%tolower) )
  d_cum <- d_run
  d_cum[selectionQobsAndSim] <- tmp_cum
  #2. (hydyearly water bilance)
  period_start <- 9
  period_end <- 8
  d_run %<>% channel.periods(start_month = period_start, end_month = period_end)
  g <- unique(d_run$period)
  require(dplyr)
  baptize <-  function(data,new_names) {
    names(data) <- new_names
    return(data)
  }
  only_q <- d_run %>% select(starts_with("q"))
  p <- matrix( data = NA, nrow =  12, ncol = (dim(d_run)[2]-7) ) %>% as.data.frame %>% 
        baptize( names(only_q) )
  for (k in 1:12) {
    p[k,] <- d_run %>% filter(mm == k) %>% select(starts_with("q")) %>% apply(.,2,sum)
  }
  # 3. test for areal accumulation 
  nb <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  to_nb <- c(2,3,4,9,6,9,9,9,12,12,12,0) 
  area <- c(44.2,47.6,28.4,11.1,34.8,5.9,87.1,67.1,49.1,58.8,62.9,154.2)
  test_case <- data.frame(nb,to_nb,area)
  #
  sum_a <- nb 
  for (n in nb) {
    if (n == 1) {
      sum_a[n] <- area[n] 
    } else {
      tmp <- test_case %>% filter(to_nb == n) %>% as.numeric 
      
      if ( is.na(tmp) ) {
        
      } else 
      sum_a[n] <- area[n] + tmp
    }
  }
  

  
  }



# test --------------------------------------------------------------------

# this is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long text
