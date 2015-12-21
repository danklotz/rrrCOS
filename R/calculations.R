visCOS.explore <- function(runoff_path,spinup,ctrl) {
  # loaded dependencies 
  require("data.table") 
  require("shiny")
  require("hydroGOF")
  require("ggplot2")
  require("xts")
  require("dplyr")
  require("grid")
  require("gridExtra")
  require("reshape2")
  #
  source("R/fetch.R")
  source("R/f_expanded_barplots.R")
  source("R/f_rasterplot_functions.R")
  ######################################################################################
  # SETUP
  ######################################################################################
  if ( !exists("ctrl") ) {
    ctrl <- list()
    # ******************
    # arbitrary presets:
    ctrl$ofoldername <- "test"
    # Interactive Overview: 
    ctrl$ctrl_span  	<- c(2009,2012) 
    # OF plot options:
    # naming:
    ctrl$yearName   	<- "Jahr" 
    # color-settings:
    ctrl$colors      		<- c('#FF3300','#f6f3a1','#005900',"purple4") 
    ctrl$clr_NSEmid  	<- 0.5 
    ctrl$OFsize       <- 5.5
  }
  
  if ( exists("runoff_path") ) {
    ctrl$pathDotRunoff <- runoff_path
  } else { 
    print("no runoff path provided, choose interactively!")
    ctrl$pathDotRunoff  <- file.choose()
  }



######################################################################################
# read in 
######################################################################################
# load runoff files
  d_raw <- data.table::fread(ctrl$pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
      as.data.frame(.)
# eliminate basins withouth observations:
  d_runoff <- sweep.NoSim(d_raw)

# get amount of used basins and their respective names
  temp_names <- names(d_runoff)
  temp_names <- gsub('.*_', '' ,temp_names) %>% unique(.) # replaces everything before "_" and gets the unique names
  eval_size <- length(temp_names)-5 
  d_nums <- temp_names[6:(5+eval_size)] %>% as.integer(.)
  d_raw_names <- names(d_raw)[6:length(d_raw)]
# remove spinup-time
  # much of this will be removed in the future, but its nice to have it for now
  # the exists arguments do not WORK THIS WAY!!!!!
  if (exists("spinup")) {
    lngth_spinup <- spinup
  } else if ( exists("pathSpinup",where = ctrl) & exists("pattern_spinup", where = ctrl) ) {
    path_Spinup <- ctrl$pathSpinup
    pattern_spinup <- ctrl$pattern_spinup
    lngth_spinup <- fetch.spinup(path_Spinup,pattern_spinup)
  } else if ( !exists("pathSpinup",where = ctrl) & exists("pattern_spinup", where = ctrl) ) {
    pathSpinup <- sweep.path(ctrl$pathDotRunoff) %>% paste("Statistics.txt", sep="") 
    pattern_spinup <- "start time-step of evaluation"
    lngth_spinup <- fetch.spinup(path_Spinup,pattern_spinup)
  } else {
    path_Spinup <- sweep.path(ctrl$pathDotRunoff) %>% paste("Statistics.txt", sep="") 
    pattern_spinup <- "start time-step of evaluation"
    lngth_spinup <- fetch.spinup(path_Spinup,pattern_spinup)
  }
  lngth_sim <- dim(d_runoff)[1] 
  d_runoff <- slice(d_runoff,lngth_spinup:lngth_sim)
# add full date information to data 
  ThereAreCOSdates <- any(names(d_runoff)=="yyyy")
  ThereArePOSIXctDates <- any(names(d_runoff)=="POSIXdate")
  if ( is.logical(ThereAreCOSdates) & is.logical(ThereArePOSIXctDates) ) {
    if (!ThereAreCOSdates & !ThereArePOSIXctDates) {
      stop("No COSdates and no POSIXct-dates in the data!")
    } else if (ThereAreCOSdates & !ThereArePOSIXctDates) {
      # add ts-dates
      d_runoff$POSIXdate <- implode.Cosdate(d_runoff)
    } else if (!ThereAreCOSdates & ThereArePOSIXctDates) {
      stop("POSIXct to COSdates not yet supported :(")
    }
  } else { 
    stop("Something seems to be wrong with the date and time formats :(")
  }
  #
# convert d_runoff to time series object (i.e. "xts")
  d_xts <- d_runoff %>% 
              dplyr::select(.,-starts_with("QOSI_")) %>%
              filter(.,yyyy >= ctrl$ctrl_span[1],yyyy <= ctrl$ctrl_span[2])
#********************************
# calculate hydrological years:
#********************************
  years_in_data <- unique(d_runoff$yyyy)
  years_in_data_shrt <- years_in_data %>% 
    as.character %>% 
    substring(.,3,4)
  num_years = length(years_in_data)
  d_runoff$hydyear <- as.character(d_runoff$POSIXdate)
  num_hydyears <- length(years_in_data_shrt) - 1
  # cut months before first year & after last year : 
  if (d_runoff$mm[1] > 9) num_hydyears <- num_hydyears - 1
  if (d_runoff$mm[lngth_sim] < 9) num_hydyears <- num_hydyears - 1
  hydyears_in_d <- years_in_data_shrt[1:num_hydyears]
  # get hydrological years
  cnt <- 0
  for (i in 1:(num_hydyears)) 
  {
    hydyears_in_d[i] <- paste(years_in_data_shrt[i],years_in_data_shrt[i+1], sep = "/")
    tmp_d_YearX <- filter(d_runoff, yyyy == years_in_data[i] | yyyy == years_in_data[i+1])  %>% 
      filter(yyyy == years_in_data[i] & mm >= 9 ) %>%
      filter(yyyy == years_in_data[i+1] & mm < 9 ) %>%
      select(hydyear) %>%
      transform(hydyear = hydyears_in_d[i])
    tmp_lngth <- dim(tmp_d_YearX)[1]
    d_runoff$hydyear[(cnt+1):(cnt+tmp_lngth)] <- tmp_d_YearX$hydyear
    cnt = cnt + tmp_lngth
  }
  rm(tmp,idx_temp,tmp_lngth,tmp_d_YearX)
  
  
  ######################################################################################
  # total & yearly (hydyears) evalution of the NSE, KGE,  percentage Bias & Correlation
  ######################################################################################
  # calculations:
  NSE_hydyearly <- matrix(nrow = num_hydyears, ncol = as.integer(eval_size), data = NA)
  KGE_hydyearly <- NSE_hydyearly 
  pBias_hydyearly <- NSE_hydyearly
  cor_hydyearly <- NSE_hydyearly
  for (k in 1:num_hydyears) 
  {
    tempOBS <- filter(d_runoff,hydyear == hydyears_in_d[k]) %>% select(.,starts_with("QOBS_"))
    tempSIM <- filter(d_runoff,hydyear == hydyears_in_d[k]) %>% select(.,starts_with("QSIM_"))
    NSE_hydyearly[k,1:eval_size] <- hydroGOF::NSE(tempSIM,tempOBS)
    KGE_hydyearly[k,1:eval_size] <- hydroGOF::KGE(tempSIM,tempOBS)
    pBias_hydyearly[k,1:eval_size] <- hydroGOF::pbias(tempSIM,tempOBS)
    cor_hydyearly[k,1:eval_size] <- cor(tempSIM,tempOBS) %>% diag(.)
  }
  tempOBS <- select(d_runoff,starts_with("QOBS_"))
  tempSIM <- select(d_runoff,starts_with("QSIM_"))
  NSE_total <- hydroGOF::NSE(tempSIM,tempOBS)
  KGE_total <- hydroGOF::KGE(tempSIM,tempOBS)
  pBIAS_total <- hydroGOF::pbias(tempSIM,tempOBS)
  cor_total <- cor(tempSIM,tempOBS) %>% diag(.)
  # some cleaning
  rm(tempOBS,tempSIM)
  # write out NSE .txt & total text files
  pathtoOut <- paste("out/",sep = "")
  write.table(cbind(d_nums,t(NSE_hydyearly), NSE_total),
              file = paste(pathtoOut,"NSE_Hydyear.csv", sep = ""),
              row.names = FALSE, col.names = c("#",paste("HY",hydyears_in_d),"TOTAL"), quote = FALSE, sep = ";")
  cbind(d_nums,NSE_total,KGE_total,pBIAS_total,cor_total) %>%
    write.table(.,file = paste(pathtoOut,"OF_total.csv", sep = ""),
                row.names = FALSE, 
                col.names = c("#","NSEtotal","KGEtotal","pBIAStotal","CORRtotal"),
                sep = ";")
  rm(pathtoOut)
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
  plt_exp_NSE <- list_yOF_barplts(NSE_hydyearly,eval_size,d_nums,d_OFyearly,plt_ctrl)
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
  plt_exp_pBias <- list_yOF_barplts(pBias_hydyearly,eval_size,d_nums,d_OFyearly,plt_ctrl)
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
  plt_exp_KGE <- list_yOF_barplts(KGE_hydyearly,eval_size,d_nums,d_OFyearly,plt_ctrl)
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
  plt_ctrl$lb_cut <- -1.0
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
  plt_exp_cor <- list_yOF_barplts(cor_hydyearly,eval_size,d_nums,d_OFyearly,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  s_ctrl$hmtlfilename <- "expnd_cor"
  s_ctrl$jpgfilename <- "expnd_cor"
  save_expnd_barplts(plt_exp_cor,eval_size,s_ctrl)
  
  ######################################################################################
  # now I need to run the app somehow!!!
  ######################################################################################
}
