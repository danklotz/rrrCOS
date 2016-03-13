#§
# DONT USE THIS
# Temporary test file, will not be used in future
# to be replaced with Examples1.Rmd
#§

visCOS.example <- function(runoff_path,spinup,ctrl) {
  require(magrittr, quietly = TRUE)
  # data wrangling --------------------------------------------------------------
  # SETUP #§ temporary !?
   ctrl <- fetch_ctrl()
   ctrl$pathDotRunoff  <- file.choose()

  # load runoff files
  
  #§ assumed to be done by the user!!!
        require("data.table")
        d_raw <- fread(ctrl$pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
             as.data.frame(.)
        names(d_raw)[5] <- "min"
  #§
        d_raw <- fetch_runoff_example()
        #§
  # eliminate basins withouth observations:
   d_runoff <- d_raw %>% 
    channel_remove_chunk %>% 
    channel_only_observed
  # get num of used basins and their respective num
  #§ shall I wrap this into a channel function??
  num_basins <- fetch_number_of_basins(d_runoff)
  # remove spinup-time
  #§ use this later in the examples:
  #  path_Spinup <- channel_path(ctrl$pathDotRunoff) %>% paste("Statistics.txt", sep="")
  #  pattern_spinup <- "start time-step of evaluation"
  #  spinup <- fetch_spinup(path_Spinup,pattern_spinup)
  #  d_runoff <- slice( d_runoff,spinup:dim(d_runoff)[1] )
  #§
  
  # add full date information to data
  d_runoff$POSIXdate <- channel_implode_cosdate(d_runoff)
  # normalize data names: 
  d_runoff %<>% channel_names
  
  # convert d_runoff to time series object (i.e. "xts")
  d_xts <- channel_runoff_as_xts(d_runoff)
  # calculate hydrological years:
  d_runoff <- channel_periods(d_runoff, start_month = 9, end_month = 8)
  years_in_data <- fetch_years_in_data(d_runoff)
  #§ its not realy smart to handle it like this, whith two strange variables. Maybe better solution possible?
    # hydyears_in_d <- fetch_hydyears(d_runoff,years_in_data) #§ this is / was stupid, was it not?
  periods_in_data <- which(unique(d_runoff$period) > 0)
  num_periods <- length(periods_in_data)
  

# calculations ------------------------------------------------------------
  bOF <- fetch_period_ofun(d_runoff)
  
  
  
# plotting --------------------------------------------------------------
  ## NSE
  ### yearly
    plt_ynse <- pour_period_NSE(from = bOF, given = periods_in_data)
  ### total
    plty_tnse <- pour_totalNSE(from = bOF)
  #### concatenate two 
    g1 <- ggplotGrob(plt_ynse)
    g2 <- ggplotGrob(plty_tnse)
    grob_pltynse <- set_panel_size(g = g1, width = unit(0.6,"npc"),height=unit(0.8,"npc"))
    grob_pltytnse <- set_panel_size(g = g2, width = unit(0.1,"npc"),height=unit(0.8,"npc"))
    require(gtable)
    plot_both <- cbind(grob_pltynse,grob_pltytnse, size = "first")
    grid.newpage()
    grid.draw(plot_both)

    
    
    
  ### expanded barplots & htmlfiles
  # fetch new list:
  plt_ctrl <- fetch_plt_ctrl()
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "NSE"
  #
  plt_exp_NSE <- pour_expanded_barplots(Ofun_hydyearly,hydyears_in_data,num_basins,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  pour_expandedbars_intoFile(plt_exp_NSE, path = "",jpg_filenames = "expnd_nse", hmtl_filename = "summary_expnd_nse")

  #§ here we go ... 
  
  # plots: %-bias -----------------------------------------------------------
  #
  pour_hydyearly_pBIAS(from = bOF, given = hydyears_in_d)
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
  plt_ykge <- pour_hydyearly_KGE(from = bOF, given = hydyears_in_d)
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
  pour_hydyearly_Corr(from = bOF, given = hydyears_in_d)
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
  # d_run <- fetch_runoff_example() %>% channel_remove_chunk
  ctrl <- fetch_ctrl()
  ctrl$pathDotRunoff  <- file.choose()
  require("data.table")
  d_raw <- fread(ctrl$pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
    as.data.frame(.)
  names(d_raw)[5] <- "min"
  d_run <- d_raw %>% 
    channel_remove_chunk %>% 
    channel_only_observed  
  tmp_cum <- d_run %>%
    select(starts_with("qobs"), starts_with("qsim")) %>%
    apply(.,2,cumsum) %>%
    as.data.frame
  selectionQobsAndSim <-  grepl( "qobs.*|qsim.*",(names(d_run)%>%tolower) )
  d_cum <- d_run
  d_cum[selectionQobsAndSim] <- tmp_cum
  #2. (hydyearly water bilance)
  require(dplyr, quietly = TRUE)
  require(ggplot2, quietly = TRUE)
  require(broom, quietly = TRUE)
  require(GGally, quietly = TRUE)
  require(grid, quietly = TRUE)
  # 
  period_start <- 9
  period_end <- 8
  d_run %<>% channel_periods(start_month = period_start, end_month = period_end)
  g <- unique(d_run$period)
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
  # future function for plotting: 
  obs_name <- "QOBS_0085"
  sim_name <- "QSIM_0085"
  q_data <- p %>% select( contains(obs_name) , contains(sim_name) )
  names(q_data) <- c("obs","sim") 
  q_data %<>% mutate(rel_error = 100*(sim-obs)/obs) 
  # include list of monts
  mean_error <- mean(q_data$rel_error)
  q_data <- rbind(  q_data,c(NA,NA,mean_error) )
  month <- c(1:13) %>% as.factor(.)
  q_data <- cbind(month,q_data) 
  levels(month)[13] <- "mean"
  #

  
  plots <- list()
  p1 <- ggplot(q_data) + 
    geom_line( aes_string(x = "month", y = "obs", group = 1), color = "steelblue", na.rm = TRUE)  + 
    geom_line( aes_string(x = "month", y = "sim", group = 2), color = "palevioletred", na.rm = TRUE ) + 
    theme_bw()

  p2 <- ggplot(q_data, aes(x = month, y = rel_error )) + 
    scale_y_continuous(limits = c(-100,100)) + 
    geom_bar(fill = "orange", position = "identity", stat = 'identity') + 
    theme_bw()
  
  p11 <- ggplotGrob(p1)
  p22 <- ggplotGrob(p2)
  
  p111 <- set_panel_size(g = p11, width = unit(0.8,"npc"),height=unit(0.5,"npc"))
  p222 <- set_panel_size(g = p22, width = unit(0.8,"npc"),height=unit(0.25,"npc"))
  
  graphic1 <- rbind(p111, p222, size="first")
  graphic1$widths <- unit.pmax(p111$widths, p222$widths)
  
  plot.new()
  grid.draw(graphic1)
  # 
  
  # for later, to rescale to mm 
    cum_area <- function(areal_data) {
      # def 
        require(dplyr, quietly = TRUE)
        # missing :((((
      # calc:
      names(areal_data) <- c("nb","to_nb","area")
      areal_data %<>% mutate(cum_area = area)
      # 
      for (n in areal_data$nb) {
        tmp <- areal_data %>% filter(to_nb == n) %>% select(cum_area) %>% apply(.,2,sum)
        areal_data$cum_area[n] <- areal_data$area[n] + tmp
      } 
      return(areal_data)
    }
  
    links <- read.csv("in/ezfl_links.txt", header = TRUE, sep = ";")
    links  %<>% cum_area
  
# 3. test for areal accumulation  -----------------------------------------
  ## test 1
    nb <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    to_nb <- c(2,3,4,9,6,9,9,9,12,12,12,0) 
    area <- c(44.2,47.6,28.4,11.1,34.8,5.9,87.1,67.1,49.1,58.8,62.9,154.2)
    sum_area.real <- c(44.2,91.9,120.3,131.3,34.8,40.7,87.1,67.1,375.3,58.8,62.9,651.3)
    test_case <- data.frame(nb,to_nb,area)
    #
    test_case %<>% cum_area
    #
    test <- equals(sum_area.real,test_case$cum_area) # weak, because of rounding errors!
    stopifnot(min(test) == 1)
  ## test2 
    test_case2 <- read.csv("in/ezfl_links.txt", header = TRUE, sep = ";")
    test_case2  %<>% cum_area
    # cannot be made properly because johannes-data is wrong :(
  
  
  
  }






# test --------------------------------------------------------------------

# this is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long text
