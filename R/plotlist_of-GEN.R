#' plotlists for the objective functions 
#'
#' To be written soon
#' 
#' @import magrittr
#' @import reshape2
#' @import ggplot2 
#' 
#' @export
plotlist_of <- function(runoff_data,
                     kind = "barplot_of") {
  # def missing ! 
  
  # calc 
  of <- extract_objective_functions(runoff_data)
  if (kind == "barplot_of") {
    plot_list <- serve.plotlist_barplot(of)
  } else if(kind == "rasterplot_of") {
    plot_list <- serve.plotlist_raster(of)
  } else {
    stop("no such option for the kind of plot")
  }
  return(plot_list)
}
  

serve.plotlist_barplot <- function(of) {
  #################
  # calculate objective functions
  num_basins <- ncol(of) %>% subtract(1)
  # define plot function: 
  melted_user_of <- suppressMessages( reshape2::melt(of) )%>% 
    mutate(of_group = ifelse(of %>% as.character %>% startsWith(.,"NSE"),"NSE",
                             ifelse(of %>% as.character %>% startsWith(.,"KGE"),"KGE",
                                    ifelse(of %>% as.character %>% startsWith(.,"CORR"),"CORR",
                                           ifelse(of %>% as.character %>% startsWith(.,"pBIAS"),"pBIAS",NA))))) 
  groupings_of <- melted_user_of$of_group %>% unique
  # define plot list function
  plotlist_fun_barplot <- function(grouping) {
    of_to_plot <- melted_user_of %>% filter( of_group == grouping)
    if (grouping == "pBIAS") {
    plt_out <- ggplot(data = of_to_plot) +
      geom_bar(stat = "identity",
               position = "identity",
               aes(x = of,
                   y = value)) +
      ggtitle(grouping) +
      facet_wrap(~ variable, ncol = 1) + 
      ylim(c(-100,100))
    return(plt_out)
    } else {
    plt_out <- ggplot(data = of_to_plot) +
      geom_bar(stat = "identity",
               position = "identity",
               aes(x = of,
                   y = value)) +
      facet_wrap(~ variable, ncol = 1) + 
      ggtitle(grouping) +
      ylim(viscos_options("limits"))
    return(plt_out)
    }
  }
  # map plotlist_fun
  plot_list <- lapply(groupings_of,plotlist_fun_barplot)
  names(plot_list) <- c("NSE","KGE","pBIAS","CORR")
  return(plot_list)
}
# raster hydyearly NSE ----------------------------------------------------
# NSE raster plot for the available hydrological years
# xxx description to follow 
serve.plotlist_raster <- function(of) {
  # 
  regex_of <- c("NSE.*","KGE.*","pBIAS.*","CORR.*")
  plot_list <- lapply(regex_of,function(x) plot_fun_raster(x,of)) %>% 
    set_names(c("NSE","KGE","pBIAS","CORR"))
  return(plot_list)
}
  plot_fun_raster <- function(regex_single_of,of) {
    period_characters <- rep("period",(ncol(of) - 1))
    if (regex_single_of == "pBIAS.*") {
      gglimits <- c(-viscos_options("limits")[2]*100,
                    viscos_options("limits")[2]*100)
      ggcolors <- c(viscos_options("color_of_mid"),
                    viscos_options("color_of_high"),
                    viscos_options("color_of_low"))
    } else {
      gglimits <- viscos_options("limits")
      ggcolors <- c(viscos_options("color_of_low"),
                    viscos_options("color_of_mid"),
                    viscos_options("color_of_high"))
    }
    # 
    plot_data <- of %>% 
      extract(grep(regex_single_of,.$of), ) %>% 
      cbind(.,facets = c("overall",period_characters)) %>% 
      reshape2::melt(., id.vars = c("of","facets")) %>% 
      reverse_basin_levels %>% 
      reverse_facetting_levels %>%
      dplyr::mutate(value = pmax(value,gglimits[1])) %>% 
      dplyr::mutate(value = pmin(value,gglimits[2])) %>% 
      dplyr::mutate(value = round(value,2))
    #
    plt_out <- ggplot(plot_data, aes(of,variable, fill = value),environmnet = environment()) +
      geom_raster(position = "identity") +
      coord_fixed(ratio = 5)  + 
      facet_grid(~ facets,  scales = "free_x", space = "free") +
      scale_fill_gradient2(space = "Lab",
                           low = ggcolors[1],
                           mid= ggcolors[2], 
                           high = ggcolors[3],
                           na.value = viscos_options("color_of_out")) +
      theme( legend.position="none" )  +
      geom_tile(color = "white", size = 0.25 ) +
      geom_text(aes(of,variable, label = as.character(value,2)), color= "black") 
    return(plt_out)
  }

  reverse_basin_levels <- function(plot_data) {
    plot_data$variable <- factor(plot_data$variable,
                                 levels = plot_data$variable %>%
                                   levels() %>% 
                                   rev() )
    return(plot_data)
  }
  reverse_facetting_levels <- function(plot_data) {
    plot_data$facets <- factor(plot_data$facets,
                                 levels = plot_data$facets %>%
                                   levels() %>% 
                                   rev() )
    return(plot_data)
  }
