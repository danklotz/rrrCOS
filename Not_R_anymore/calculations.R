#§
# DONT USE THIS
# Temporary test file, will not be used in future
# to be replaced with Examples1.Rmd
#§

visCOS.example <- function(runoff_path,spinup,ctrl) {
  # data wrangling --------------------------------------------------------------
  # SETUP #§ temporary !?
   pathDotRunoff  <- file.choose()

  # load runoff files
  
  #§ assumed to be done by the user!!!
        require("data.table")
        require("magrittr", quietly = TRUE)
        d_raw <- fread(pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
             as.data.frame(.)
        names(d_raw)[5] <- "min"
  #§    
        require("magrittr", quietly = TRUE)
        d_raw <- pour.runoff_example()
        #§
  # eliminate chunk and basins withouth observations:
   d_runoff <- d_raw %>% remove_chunk %>% mark_periods
  # get num of used basins and their respective num
  #§ shall I wrap this into a prepare function??
  num_basins <- get_basin_numbers(d_runoff)
  # remove spinup-time
  #§ use this later in the examples:
  #  path_Spinup <- remove_filename_from_path(ctrl$pathDotRunoff) %>% paste("Statistics.txt", sep="")
  #  pattern_spinup <- "start time-step of evaluation"
  #  spinup <- pour.spinup(path_Spinup,pattern_spinup)
  #  d_runoff <- slice( d_runoff,spinup:dim(d_runoff)[1] )
  #§
  
  # add full date information to data
  d_runoff <- prepare.complete_date(d_runoff)
  # normalize data names: 
  d_runoff %<>% remove_leading_zeros
  
  # convert d_runoff to time series object (i.e. "xts")
  d_xts <- runoff_as_xts(d_runoff) ## CARe #§ should not be used anymore outside of functions
  # calculate hydrological years:
  d_runoff <- prepare.periods(d_runoff, start_month = 9, end_month = 8)
  periods_in_data <- which(unique(d_runoff$period) > 0)
  num_periods <- length(periods_in_data)

# calculations ------------------------------------------------------------
  bOF <- extract_of(d_runoff)
  
  
  
# plotting --------------------------------------------------------------
  ## NSE
  ### yearly
    plt_ynse <- periodplot_NSE(d_runoff)
  ### total
    plty_tnse <- totalplot_NSE(d_runoff)
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
  # pour new list:
  plt_ctrl <- viscos_options()
  plt_ctrl$gtitle <- "Basin"
  plt_ctrl$ylab <- "NSE"
  #
  plt_exp_NSE <- serve.plotlist_periodOF(Ofun_hydyearly,hydyears_in_data,num_basins,plt_ctrl)
  # save formated list into htmlFile  (cause shiny does not like multiple graphics)
  s_ctrl <- list() # reset save control (s_ctrl)
  serve.save_plotlist3x3(plt_exp_NSE, path = "",jpg_filenames = "expnd_nse", hmtl_filename = "summary_expnd_nse")

  #§ here we go ... 
  
  # plots: %-bias -----------------------------------------------------------
  #
  serve.hydyearly_pBIAS(from = bOF, given = hydyears_in_d)
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
  plt_ykge <- serve.hydyearly_KGE(from = bOF, given = hydyears_in_d)
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
  serve.hydyearly_Corr(from = bOF, given = hydyears_in_d)
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
  require("magrittr", quietly = TRUE)
  # d_run <- pour.runoff_example() %>% prepare.remove_chunk
  pathDotRunoff  <- file.choose()
  require("data.table", quietly = TRUE)
  d_raw <- fread(pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
    as.data.frame(.)
  names(d_raw)[5] <- "min"
  d_run <- d_raw %>% 
    prepare.remove_chunk 
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
  # remove spinup
  spinup <- 5831
  d_run <- slice( d_runoff,spinup:dim(d_runoff)[1] )
  # set periods
  period_start <- 9
  period_end <- 8
  d_run %<>% prepare.periods(start_month = period_start, end_month = period_end)
  #3. change to mm
  # for later, to rescale to mm 
  cum_area <- function(areal_data) {
    # def 
    require(dplyr, quietly = TRUE)
    #§ more def?
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
  
  names_drun <- d_run  %>% select(starts_with("Q")) %>% names(.)
  d_run_mm <- d_run
  for (my_var in names_drun) {
    idx <- gsub("\\D","",my_var) %>% as.integer
    to_mm <- function(x) { x*3.6/(links$cum_area[idx]) }
    call <- substitute(transmute(d_run, var = to_mm(var)), list(var = as.name(my_var)))
    d_run_mm[my_var] <- eval(call)
  }

  
  
  # 4. water balance
  g <- unique(d_run_mm$period)
  baptize <-  function(data,new_names) {
    names(data) <- new_names
    return(data)
  }
  only_q <- d_run_mm %>% select(starts_with("q"))
  years <- unique(d_run_mm$yyyy)
  p <- matrix( data = 0, nrow =  12, ncol = (dim(d_run_mm)[2]-7) ) %>% as.data.frame %>% 
        baptize( names(only_q) )
  for (idx in 1:length(years)) {
    now <- years[idx]
    for (mon in 1:12) {
      p[mon, ] <- 1/idx* (  p[mon, ]*(idx-1) +
        d_run_mm %>% filter(yyyy == now) %>% filter(mm == mon) %>% select(starts_with("q")) %>% apply(.,2,sum)  )
    }
  }


  # future function for plotting: 
  obs_name <- "QOBS_0002"
  sim_name <- "QSIM_0002"
  q_data <- p %>% select( contains(obs_name) , contains(sim_name) )
  names(q_data) <- c("obs","sim") 
  q_data %<>% mutate(rel_error = 100*(sim-obs)/obs) 
    # include list of months

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
    theme_light()

  p2 <- ggplot(q_data, aes(x = month, y = rel_error )) + 
    scale_y_continuous(limits = c(-100,100)) + 
    geom_bar(fill = "orange", position = "identity", stat = 'identity') + 
    theme_light()
  
  p11 <- ggplotGrob(p1)
  p22 <- ggplotGrob(p2)
  
  # set panel size can be found in the helpers section!

  p111 <- set_panel_size(g = p11, width = unit(0.8,"npc"),height=unit(0.5,"npc"))
  p222 <- set_panel_size(g = p22, width = unit(0.8,"npc"),height=unit(0.25,"npc"))
  
  graphic1 <- rbind(p111, p222, size="first")
  graphic1$widths <- unit.pmax(p111$widths, p222$widths)
  
  plot.new()
  grid.draw(graphic1)
  # 
  


  
  
  
  }






# test --------------------------------------------------------------------

# this is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long textthis is a long textthis is a long
# textthis is a long textthis is a long text
