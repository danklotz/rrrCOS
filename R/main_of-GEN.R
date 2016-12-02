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
main_of_compute <- function(runoff_data) {
  assert_dataframe(runoff_data)
  if( !exists(viscos_options("name_COSperiod"), where = runoff_data) ) {
    stop("Error! Period-Column missing in runoff_data; use `mark_periods`")
  }
  evaluation_data <- 
    runoff_data[ runoff_data[[viscos_options("name_COSperiod")]] > 0, ]
  number_of_basins <- evaluation_data %>%
    names %>%
    unique %>%
    grepl(viscos_options("name_o"), ., ignore.case = TRUE) %>%
    sum 
  periods_in_data <- evaluation_data %>%
    magrittr::extract2(viscos_options("name_COSperiod")) %>% 
    unique
  number_of_periods <- periods_in_data %>% length
  temp_x <- dplyr::select(evaluation_data,starts_with(viscos_options("name_o"))) %>%
    unname
  temp_y <- dplyr::select(evaluation_data,starts_with(viscos_options("name_s"))) %>%
    unname
  nse_ <- hydroGOF::NSE(temp_y,temp_x)
  kge_ <- hydroGOF::KGE(temp_y,temp_x)
  pbias_ <- hydroGOF::pbias(temp_y,temp_x)
  corr_ <- cor(temp_y,temp_x) %>% diag(.)

  # Calulcated period-vise objective functions
    # pre allocation of periodic variables:
    NSE_period <- matrix(nrow = number_of_periods, ncol = as.integer(number_of_basins), data = NA)
    KGE_period <- NSE_period
    pBIAS_period <- NSE_period
    CORR_period <- NSE_period
    # calculation loop # proabbly slow
    for (k in 1:number_of_periods) {
      temp_x <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options("name_o"))) %>%
        unname
      temp_y <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options("name_s"))) %>%
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
main_of_barplot <- function(runoff_data) {
  main_of_names <- c("NSE","KGE","CORR","pBIAS")
  assert_dataframe(runoff_data)
  of <- main_of_compute(runoff_data)
  # calculate objective functions
  num_basins <- ncol(of) - 1
  # melt the of:
  # a bit of a mess, and there is probaly a better solution...
  melted_of <- suppressMessages( reshape2::melt(of) ) %>% 
    mutate(of_group = ifelse(of %>% as.character %>% startsWith(.,main_of_names[1]),main_of_names[1],
                             ifelse(of %>% as.character %>% startsWith(.,main_of_names[2]),main_of_names[2],
                                    ifelse(of %>% as.character %>% startsWith(.,main_of_names[3]),main_of_names[3],
                                           ifelse(of %>% as.character %>% startsWith(.,main_of_names[4]),main_of_names[4],NA))))) 
  # define plot-list function
  plotlist_fun_barplot <- function(of_name) {
    of_to_plot <- melted_of %>% filter( of_group == of_name)
    if (of_name == "pBIAS") {
      gglimits <- c(-viscos_options("of_limits")[2]*100,
                   viscos_options("of_limits")[2]*100)

    } else {
      gglimits <- viscos_options("of_limits")
    }
    plt_out <- ggplot(data = of_to_plot) +
      geom_bar(stat = "identity",
               position = "identity",
               aes(x = of,
                   y = value, 
                   fill = value)) +
      facet_wrap(~ variable, ncol = 1) + 
      ggtitle(of_name) +
      ylim(gglimits)
    return(plt_out)
  }
  # apply plot.list function to the different groupings
  plot_list <- lapply(main_of_names,plotlist_fun_barplot)
  names(plot_list) <- main_of_names
  return(plot_list)
}
#' Barplot for the Main Objective Function Values 
#' 
#' @rdname of_overview
#' @import pasta
#' @export
main_of_rasterplot <- function(runoff_data) {
  main_of_names <- c("NSE","KGE","CORR","pBIAS")
  regex_main_of <- main_of_names %&% ".*"
  assert_dataframe(runoff_data)
  of <- main_of_compute(runoff_data)
  #
  plot_list <- lapply(regex_main_of,function(x) plot_fun_raster(x,of)) %>% 
    set_names(c("NSE","KGE","pBIAS","CORR"))
  return(plot_list)
}
  plot_fun_raster <- function(regex_single_of,of) {
    if (regex_single_of == "pBIAS.*") {
      gglimits <- c(-viscos_options("of_limits")[2]*100,
                    viscos_options("of_limits")[2]*100)
    } else {
      gglimits <- viscos_options("of_limits")
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
    plt_out <- ggplot(plot_data, aes(of,variable, fill = value), environmnet = environment()) +
      geom_raster(position = "identity") +
      coord_fixed(ratio = 5)  + 
      facet_grid(~ facets,  scales = "free_x", space = "free") +
      theme( legend.position = "none")  +
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
