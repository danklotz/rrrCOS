  require(magrittr)
  require(visCOS)
  require(ggplot2)
  library(Hmisc)
  library(dplyr)
  library(hydroGOF)
  # 
  runoff_data <- get_runoff_example() %>% 
    remove_chunk() %>% 
    mark_periods()
  data1 <- data.frame(x = 1:length(runoff_data$QOBS_0001),
                     obs = runoff_data$QOBS_0001, 
                     sim = runoff_data$QSIM_0001,
                     period = runoff_data$period)
  # 
  data1$cut_marks <- data1$obs %>% 
    Hmisc::cut2(., g = 3) %>% 
    as.numeric
  data1$group <- c( 0,diff(data1$cut_marks) ) %>%
    equals(0) %>% 
    not %>% 
    cumsum() %>% 
    add(1)
  
  calc_nse <- function(obs,sim) {
    1 - sum((obs - sim)^2)/sum((obs-mean(obs))^2)
  }
  
  grouped_data1 <- data1 %>% 
    filter(period > 0) %>% 
    group_by(group) 
  
  %>% 
    summarise(x = mean(x),
              period = min(period),
              mark = mean(cut_marks), 
              obs = mean(obs), 
              sim = mean(sim), 
              nse = mean(abs(obs-sim))) 
  ggplot() + 
    geom_point(data = grouped_data1, 
               aes(x = x, y = sim, color = nse), 
               alpha = 0.5, 
               size = 5) +
    scale_color_gradient(low = "steelblue", high = "red") +  + 
    facet_wrap(~period, scale = "free")
  

  