#' create list of water-bilance plots 
#' 
#' xxx
#' @export
serve.plotlist_waterbalance <- function(runoff_data, cum_area_in_km = NULL, calculate_mm = TRUE) {
  #def
    require("dplyr", quietly = TRUE)
    #
    assert_chunk(runoff_data)
  # calc
  runoff_data_mm <- runoff_data
  if (calculate_mm == TRUE) {
    stopifnot(!is.null(cum_area_in_km)) # stop if no area is provided
    # calculate runoff in mm:
    names_drun <- runoff_data  %>% select(starts_with("Q")) %>% names(.)
    for (my_var in names_drun) {
      idx <- gsub("\\D","",my_var) %>% as.integer
      to_mm <- function(x) { x*3.6/(cum_area_in_km[idx]) }
      # strings cannot be evaluated directly in dplyr, thus a little trick is needed
      call <- substitute(transmute(d_run, var = to_mm(var)), list(var = as.name(my_var)))
      runoff_data_mm[my_var] <- eval(call)
    }
  }
  #
  obs_names <- names(runoff_data) %>% tolower %>% extract(grep("qobs.*",.))
  plot_list <- list()
  for (i in 1:length(obs_names)) {
    plot_list[[i]] <- serve.waterbalance(runoff_data_mm, obs_name = obs_names[i], cum_area_in_km = NULL, calculate_mm = FALSE)
  }
  return(plot_list)
} 


# wrapper to plot water bilance graphic object ----------------------------
#' water bilance plot
#' 
#' pltos the water bilance of a chosen basin
#' @export
serve.plot_waterbalance <- function(runoff_data, obs_name, cum_area_in_km = NULL, calculate_mm = TRUE) {
  plot.new()
  grid.draw(  serve.waterbalance(runoff_data, obs_name, calculate_mm, cum_area_in_km)  )
}


# single water bilance graphic object -------------------------------------
#' makes plot for the water bilance
#' 
#' xxx
#' @export
serve.waterbalance <- function(runoff_data, obs_name, cum_area_in_km = NULL, calculate_mm = TRUE) {
  # def 
    require("dplyr", quietly = TRUE)
    require("ggplot2", quietly = TRUE)
    require("grid", quietly = TRUE)
    assert_chunk(runoff_data)
    #§ missing
  # calc
  runoff_data_mm <- runoff_data
  if (calculate_mm == TRUE) {
    stopifnot(!is.null(cum_area_in_km)) # stop if no area is provided
    # calculate runoff in mm:
    names_drun <- runoff_data  %>% select(starts_with("Q")) %>% names(.)
    for (my_var in names_drun) {
      idx <- gsub("\\D","",my_var) %>% as.integer
      to_mm <- function(x) { x*3.6/(cum_area_in_km[idx]) }
      # strings cannot be evaluated directly in dplyr, thus a little trick is needed
      call <- substitute(transmute(d_run, var = to_mm(var)), list(var = as.name(my_var)))
      runoff_data_mm[my_var] <- eval(call)
    }
  }
  # make water bilance (realy slow right now)
  g <- unique(runoff_data_mm$period)
  baptize <-  function(data,new_names) {
      names(data) <- new_names
      return(data)
  }
  only_q <- runoff_data_mm %>% select(starts_with("q"))
  years <- unique(runoff_data_mm$yyyy)
  p <- matrix( data = 0, nrow =  12, ncol = (dim(runoff_data_mm)[2]-7) ) %>% as.data.frame %>% 
  baptize( names(only_q) )
  for (idx in 1:length(years)) {
    now <- years[idx]
    for (mon in 1:12) {
      p[mon, ] <- 1/idx* (  p[mon, ]*(idx-1) +
                              runoff_data_mm %>% filter(yyyy == now) %>% filter(mm == mon) %>% select(starts_with("q")) %>% apply(.,2,sum)  )
    }
  }
  # plotting:
  sim_name <- obs_name %>% tolower %>% gsub("qobs","qsim",.)
    #§ code is a little bit shitty : (
  q_data <- p %>% select( ends_with(obs_name) , ends_with(sim_name) )
  names(q_data) <- c("obs","sim") 
  q_data %<>% mutate(rel_error = 100*(sim-obs)/obs) 
  # include list of months
  mean_error <- mean(q_data$rel_error)
  q_data <- rbind(  q_data,c(NA,NA,mean_error) )
  month <- c(1:13) %>% as.factor(.)
  q_data <- cbind(month,q_data) 
  levels(month)[13] <- "mean"
  
  # arranging plots is still quite cumbersome, thus we need to trick a bit
  plots <- list()
  plot_name <- paste("basin",gsub("qobs_","",obs_name), sep ="") 
  p1 <- ggplot(q_data) + 
    geom_line( aes_string(x = "month", y = "obs", group = 1), color = "steelblue", na.rm = TRUE)  + 
    geom_line( aes_string(x = "month", y = "sim", group = 2), color = "palevioletred", na.rm = TRUE ) + 
    ggtitle(plot_name) + 
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
    
    # plot.new()
    return(graphic1)
    
    
}