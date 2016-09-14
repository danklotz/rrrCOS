#' plotlists for the objective functions 
#'
#' To be written soon
#' @export
listplot <- function(runoff_data,
                     kind_of_plot = "barplot",
                     of_name = "NSE_period") {
  if (kind_of_plot == "barplot") {
    plot_list <- serve.plotlist_periodOF(runoff_data,of_name)
  } else {
    stop("no such option for the kind of plot")
  }
  return(plot_list)
}
  

serve.plotlist_periodOF <- function(runoff_data, of_name = "NSE_period") {
  require("ggplot2", quietly = TRUE)
  require("magrittr", quietly = TRUE)
  #################
  # calculate objective functions
  of <- extract_objective_functions(runoff_data)
  # 
  num_basins <- ncol(of) %>% subtract(1)
  plot_titles <- paste(viscos_options("plot_title"),1:num_basins,sep = "")
  user_of <- of %>% 
    extract(2:ncol(of)) %>% 
    apply(.,2, function(x) pmax(x,viscos_options("limits")[1])) %>% 
    apply(.,2, function(x) pmin(x,viscos_options("limits")[2])) %>%
    cbind.data.frame(of = of$of,.)
  
  # user_of <- of[grepl(of_name,of$of, ignore.case = TRUE), ]
  # define plot function: 
  melted_user_of <- suppressMessages( reshape2::melt(of) )%>% 
    mutate(of_group = ifelse(of %>% as.character %>% startsWith(.,"NSE"),"NSE",
                             ifelse(of %>% as.character %>% startsWith(.,"KGE"),"KGE",
                                    ifelse(of %>% as.character %>% startsWith(.,"CORR"),"CORR",
                                           ifelse(of %>% as.character %>% startsWith(.,"pBIAS"),"pBIAS",NA))))) 
  groupings_of <- melted_user_of$of_group %>% unique
  # plot list function
  plotlist_fun <- function(grouping) {
    of_to_plot <- melted_user_of %>% filter( of_group == grouping)
    if (grouping == "pBIAS") {
    plot_list <- ggplot(data = of_to_plot) +
      geom_bar(stat = "identity",
               position = "identity",
               aes(x = of,
                   y = value)) +
      ggtitle(grouping) +
      facet_wrap(~ variable, ncol = 1) + 
      ylim(c(-100,100))
    return(plot_list)
    } else {
    plot_list <- ggplot(data = of_to_plot) +
      geom_bar(stat = "identity",
               position = "identity",
               aes(x = of,
                   y = value)) +
      facet_wrap(~ variable, ncol = 1) + 
      ggtitle(grouping) +
      ylim(viscos_options("limits"))
    return(plot_list)
    }

  }
  
  
  # map plotlist_fun
  plot_list <- lapply(groupings_of,plotlist_fun)
  names(plot_list) <- c("NSE","KGE","pBIAS","CORR")
  return(plot_list)
}

