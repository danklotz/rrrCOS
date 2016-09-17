require(magrittr)
require(ggplot2)
require(dplyr)

'%&%' <- function(a,b) paste(a,b, sep = '')
'%|%' <- function(a,b) paste(a,b, sep = "|")
  runoff_example <- get_runoff_example() %>% visCOS::prepare_complete_date()
  temp_runoff <- cbind.data.frame(runoff_example %>% remove_chunk(), 
                                  date_selection = substr(runoff_example$posixdate, 6,7) %>% # 11 for daily
                                   as.factor())
  regex_for_runoff_selection <- viscos_options("name_data1") %|% viscos_options("name_data2")
  names_runoff_selection <- grep(regex_for_runoff_selection,names(temp_runoff) %>% tolower, value = TRUE)
  runoff_selection <- grep(regex_for_runoff_selection,names(temp_runoff) %>% tolower)
  # define aggregation function
  aggregator_fun <- function(k,data_frame){
    aggregation <- aggregate(data_frame[[k]] ~ data_frame$date_selection, FUN=median)
    return(aggregation[ ,2])
  }
  
  g <- sapply(runoff_selection, function(x) aggregator_fun(x,temp_runoff)) %>% 
    data.frame(., timestep = 1:nrow(.), month = unique(temp_runoff$date_selection))
  names(g) <- c(names_runoff_selection,"timestep","month")
  gg <- g %>% reshape2::melt(., id.vars = c("timestep","month")) %>% 
    cbind.data.frame(., 
                     basin =  .$variable %>%
                       gsub(regex_for_runoff_selection,"",.) %>% 
                       gsub("\\D","",.) %>%
                       as.integer, 
                     obs_sim = .$variable %>% 
                       gsub(viscos_options("name_data1") %&% ".*",viscos_options("name_data1"),.) %>% 
                       gsub(viscos_options("name_data2") %&% ".*",viscos_options("name_data2"),.))
  ggplot(gg) + 
    geom_line(aes(x = timestep, y= value, col = obs_sim)) + 
    scale_x_discrete(limits = gg$month) + 
    facet_wrap( ~ basin,ncol = 3)
  
