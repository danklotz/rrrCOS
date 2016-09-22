  #' Aggreage and plot
  #' 
  #' Hopefully a good description will follow 
  #' @export
  aggregate_and_plot <- function( runoff_data,aggregation = "mm") {
    require("magrittr")
    require("ggplot2")
    #
    if (aggregation == "dd"){
      cutting_bounds <- c(9,11)
    } else if (aggregation == "mm") {
      cutting_bounds <- c(6,7)
    } else if (aggregation == "yyyy") {
      cutting_bounds <- c(1,4)
    } else if (aggregation == "yyyy-mm") {
      cutting_bounds <- c(1,7)
    }
    '%&%' <- function(a,b) paste(a,b, sep = '')
    '%|%' <- function(a,b) paste(a,b, sep = "|")
    regex_for_runoff_selection <- viscos_options("name_data1") %|%                                   viscos_options("name_data2")
    #
    full_runoff_data <- visCOS::prepare_complete_date(runoff_data) %>% 
      visCOS::remove_chunk()
    #
    runoff_with_aggreggation <- cbind.data.frame(
      full_runoff_data ,
      date_selection = substr(full_runoff_data$posixdate,
                              cutting_bounds[1],
                              cutting_bounds[2]) %>% as.factor()
      )
    names_runoff_selection <- grep(
      regex_for_runoff_selection,names(runoff_with_aggreggation) %>% tolower, value = TRUE
      )
    selected_runoff_rows <- grep(regex_for_runoff_selection,
                             names(runoff_with_aggreggation) %>% tolower)
    sub_selection <- sapply(selected_runoff_rows, 
                function(x)aggregator_fun(x,runoff_with_aggreggation)) %>% 
      data.frame(.,timestep=1:nrow(.),month=unique(runoff_with_aggreggation$date_selection) )
    names(sub_selection) <- c(names_runoff_selection,"timestep","month")
    # 
    melted_sub_selection <- sub_selection %>% 
      reshape2::melt(., id.vars = c("timestep","month")) %>% 
      cbind.data.frame(., 
                       basin =  .$variable %>%
                         gsub(regex_for_runoff_selection,"",.) %>% 
                         gsub("\\D","",.) %>%
                         as.integer, 
                       obs_sim = .$variable %>% 
                         gsub(viscos_options("name_data1") %&% ".*",viscos_options("name_data1"),.) %>% 
                         gsub(viscos_options("name_data2") %&% ".*",viscos_options("name_data2"),.))
    the_plot <- ggplot(melted_sub_selection) + 
      geom_line(aes(x = timestep, y= value, col = obs_sim)) + 
      scale_colour_manual(values = c(viscos_options("color_data1"),viscos_options("color_data2"))) + 
      scale_x_discrete(limits = melted_sub_selection$month, 
                       labels = abbreviate) + 
      facet_wrap( ~ basin,
                  ncol = 2, 
                  scales = "free") +
      theme(panel.margin = unit(1.5, "lines")) 
    return(the_plot)
    }
  # aggregation function
  aggregator_fun <- function(k,data_frame){
    the_aggregation <- aggregate(data_frame[[k]] ~ data_frame$date_selection, FUN=median)
    return(the_aggregation[ ,2])
  }
