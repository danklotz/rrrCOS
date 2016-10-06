# approach 1 --------------------------------------------------------------
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
  data1 <- data.frame(x = 1:length(runoff_data$QOBS_0002),
                     obs = runoff_data$QOBS_0002, 
                     sim = runoff_data$QSIM_0002,
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
  
  data1 %<>%
    filter(period > 0) %>% 
    group_by(group) %>% 
    mutate(bound_kge = max(0,KGE(sim,obs))) %>% 
    ungroup
  
  grouped_data1 <- data1 %>% 
    group_by(group) %>% 
    summarise(x = mean(x),
              period = min(period),
              mark = mean(cut_marks), 
              obs = mean(obs), 
              sim = mean(sim), 
              mean_abs_error = mean(abs(obs-sim)), 
              bound_kge = mean(bound_kge)) 
  #plot 1
  ggplot() + 
    geom_abline(intercept = 0, slope = 1, color = "grey") + 
    geom_path(data = grouped_data1, 
              aes(x = obs, y = sim),
              alpha = 0.1) + 
    geom_point(data = grouped_data1, 
               aes(x = obs, y = sim, color = bound_kge), 
               alpha = 0.5, 
               size = 5) +
    scale_color_gradient(low = "red", high = "green") + 
    expand_limits(x = 0, y = 0) +
    facet_wrap(~period, scale = "free") 
  #plot 2
  ggplot() + 
    geom_point(data = grouped_data1, 
               aes(x = bound_kge, y = sim, color = mark), 
               alpha = 0.5, 
               size = 5) +
    expand_limits(x = 0, y = 0) +
    facet_wrap(~period) 
  
  # plot 2
     ggplot() + 
       geom_ribbon(data = data1, aes(x = x, ymin = 0, ymax = obs), 
                   fill = "black") +
       geom_ribbon(data = data1, aes(x = x, ymin = 0, ymax = sim), 
                   fill = "grey", 
                   alpha = 0.5) +
       geom_line(data = data1, 
                 aes(x = x, y = sim, color = bound_kge)) + 
       scale_color_gradient(low = "red", high = "green") + 
       facet_wrap(~period, scale = "free_x") 
  # plot 3  
  ggplot() + 
    geom_ribbon(data = data1, aes(x = x, ymin = 0, ymax = obs), 
                fill = "black") +
    geom_ribbon(data = data1, aes(x = x, ymin = 0, ymax = sim), 
                fill = "white", 
                alpha = 0.75) +
    geom_step(data = grouped_data1, 
               aes(x = x, y = sim, color = bound_kge), 
               alpha = 0.5) +
    scale_color_gradient(low = "red", high = "green") + 
    facet_wrap(~period, scale = "free") 
  