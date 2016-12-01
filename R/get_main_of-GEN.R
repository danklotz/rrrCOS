#' Get basic objective function for runoff_data
#'
#' Calculate basic objective functions(NSE, KGE, percentage BIAS, Correlation) 
#' for every basin and the chosen periods.
#'
#' @param runoff_data runoff_data data.frame.
#' @return list of basic objective function evaluated for the different
#' hydrological years and over the whole timespan.
#' 
#' @import hydroGOF 
#' @import dplyr
#'
#' @export
get_main_of <- function(runoff_data) {
  assert_dataframe(runoff_data)
  if( !exists(viscos_options("name_COSperiod"), where = runoff_data) ) {
    stop("Error! Period-Column missing in runoff_data; use `mark_periods`")
  }
  # (I) reduce necessary computation
  evaluation_data <- 
    runoff_data[ runoff_data[[viscos_options("name_COSperiod")]] > 0, ]
  # (II) get information
  number_of_basins <- evaluation_data %>%
    names %>%
    unique %>%
    tolower %>%
    grepl(viscos_options("name_data1") , .) %>%
    sum
  periods_in_data <- evaluation_data[[viscos_options("name_COSperiod")]] %>%
    unique
  number_of_periods <- periods_in_data %>% length

  # (III) calculate overall objective functions
  temp_x <- dplyr::select(evaluation_data,starts_with(viscos_options("name_data1"))) %>%
    unname
  temp_y <- dplyr::select(evaluation_data,starts_with(viscos_options("name_data2"))) %>%
    unname
  nse_ <- hydroGOF::NSE(temp_y,temp_x)
  kge_ <- hydroGOF::KGE(temp_y,temp_x)
  pbias_ <- hydroGOF::pbias(temp_y,temp_x)
  corr_ <- cor(temp_y,temp_x) %>% diag(.)

  # (IV) calulcated period-vise objective functions
    # pre allocation of periodic variables:
    NSE_period <- matrix(nrow = number_of_periods, ncol = as.integer(number_of_basins), data = NA)
    KGE_period <- NSE_period
    pBIAS_period <- NSE_period
    CORR_period <- NSE_period

    # calculation loop # proabbly slow
    for (k in 1:number_of_periods) {
      temp_x <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options("name_data1"))) %>%
        unname
      temp_y <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options("name_data2"))) %>%
        unname
      NSE_period[k,1:number_of_basins] <- hydroGOF::NSE(temp_y,temp_x)
      KGE_period[k,1:number_of_basins] <- hydroGOF::KGE(temp_y,temp_x)
      pBIAS_period[k,1:number_of_basins] <- hydroGOF::pbias(temp_y,temp_x)
      CORR_period[k,1:number_of_basins] <- cor(temp_y,temp_x) %>% diag(.)
    }
  #
  obj_names <- c("NSE","KGE","pBIAS","CORR", 
                    paste("NSE_period",1:number_of_periods,sep="."), 
                    paste("KGE_period",1:number_of_periods,sep="."),
                    paste("pBIAS_period",1:number_of_periods,sep="."),
                    paste("CORR_period",1:number_of_periods,sep=".")
  )
  obj_fun <- data.frame(of = obj_names, 
                        basin = rbind(nse_,
                                      kge_,
                                      pbias_,
                                      corr_,
                                      NSE_period,
                                      KGE_period,
                                      pBIAS_period,
                                      CORR_period),
                        row.names = NULL)
  return(obj_fun)
}

#' Plot main objective fucnction values
#'
#' Currently two options for plotting the main objectives are provided by 
#' visCOS: Plotting the differnet objective functions values as a set of 
#' barplots \code{barplot_of} and plotting a summary table in form of 
#' a large raster of all the objective fucntion values \code{rasterplot_of}.
#' 
#' @name plot_main_of 
NULL 
#' Barplot for the Main Objective Function Values 
#' 
#' @rdname of_overview
#' @export
barplot_main_of <- function(runoff_data) {
  assert_dataframe(runoff_data)
  of <- get_main_of(runoff_data)
  # calculate objective functions
  num_basins <- ncol(of) %>% subtract(1)
  # melt the of:
  # a bit of a mess, and there is probaly a better solution...
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
      ylim(viscos_options("of_limits"))
    return(plt_out)
    }
  }
  # map plotlist_fun
  plot_list <- lapply(groupings_of,plotlist_fun_barplot)
  names(plot_list) <- c("NSE","KGE","pBIAS","CORR")
  return(plot_list)
}
#' Barplot for the Main Objective Function Values 
#' 
#' @rdname of_overview
#' @export
rasterplot_main_of <- function(runoff_data) {
  assert_dataframe(runoff_data)
  of <- get_main_of(runoff_data)
  #
  regex_of <- c("NSE.*","KGE.*","pBIAS.*","CORR.*")
  plot_list <- lapply(regex_of,function(x) plot_fun_raster(x,of)) %>% 
    set_names(c("NSE","KGE","pBIAS","CORR"))
  return(plot_list)
}
  plot_fun_raster <- function(regex_single_of,of) {
    if (regex_single_of == "pBIAS.*") {
      gglimits <- c(-viscos_options("of_limits")[2]*100,
                    viscos_options("of_limits")[2]*100)
      ggcolors <- c(viscos_options("color_of_mid"),
                    viscos_options("color_of_high"),
                    viscos_options("color_of_low"))
    } else {
      gglimits <- viscos_options("of_limits")
      ggcolors <- c(viscos_options("color_of_low"),
                    viscos_options("color_of_mid"),
                    viscos_options("color_of_high"))
    }
    # 
    plot_data <- of %>% 
      extract(grep(regex_single_of,.$of), ) %>% 
      cbind(.,facets = c("overall", rep("period",(nrow(.) - 1)))) %>% 
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
