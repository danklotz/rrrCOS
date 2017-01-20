#' Get basic objective function for cos_data
#'
#' Calculate basic objective functions(NSE, KGE, percentage BIAS, Correlation)
#' for every basin and the chosen periods.
#'
#' @param cos_data cos_data data.frame.
#' @return list of basic objective function evaluated for the different
#' hydrological years and over the whole timespan.
#'
#' @import hydroGOF
#' @import dplyr
#'
#' @export
main_of_compute <- function(cos_data) {
  assert_dataframe(cos_data)
  if( !exists(viscos_options("name_COSperiod"), where = cos_data) ) {
    stop("Error! Period-Column missing in cos_data, use `mark_periods`")
  }
  evaluation_data <- 
    cos_data[cos_data[[viscos_options("name_COSperiod")]] > 0, ]
  number_of_basins <- evaluation_data %>%
    names %>%
    unique %>%
    grepl(viscos_options("name_o"), ., ignore.case = TRUE) %>%
    sum
  periods_in_data <- evaluation_data %>%
    magrittr::extract2(viscos_options("name_COSperiod")) %>%
    unique
  number_of_periods <- periods_in_data %>% length
  # compute main-of for entire data: ----------------------------------------
  selected_o <- dplyr::select(evaluation_data,
                              starts_with(viscos_options("name_o"))
                              ) %>% 
    unname
  selected_s <- dplyr::select(evaluation_data,
                              starts_with(viscos_options("name_s"))
                              ) %>%
    unname
  nse_ <- hydroGOF::NSE(selected_s,selected_o)
  kge_ <- hydroGOF::KGE(selected_s,selected_o)
  p_bias_ <- hydroGOF::pbias(selected_s,selected_o)
  corr_ <- cor(selected_s,selected_o) %>% diag(.)
  # compute periodwise main-of: ---------------------------------------------
  # pre allocations
  NSE_period <- matrix(nrow = number_of_periods,
                       ncol = as.integer(number_of_basins),
                       data = NA)
  KGE_period <- NSE_period
  p_bias_period <- NSE_period
  CORR_period <- NSE_period
  # calculation loop, proabbly slow :( 
  for (k in 1:number_of_periods) {
    selected_o <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
      dplyr::select(.,starts_with(viscos_options("name_o"))) %>%
      unname
    selected_s <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
      dplyr::select(.,starts_with(viscos_options("name_s"))) %>%
      unname
    NSE_period[k,1:number_of_basins] <- hydroGOF::NSE(selected_s,selected_o)
    KGE_period[k,1:number_of_basins] <- hydroGOF::KGE(selected_s,selected_o)
    p_bias_period[k,1:number_of_basins] <- hydroGOF::pbias(selected_s,selected_o)
    CORR_period[k,1:number_of_basins] <- cor(selected_s,selected_o) %>% diag(.)
  }
  # clean up: ---------------------------------------------------------------
  obj_names <- c("NSE","KGE","p_bias","CORR",
                    paste("NSE_period",1:number_of_periods,sep="."),
                    paste("KGE_period",1:number_of_periods,sep="."),
                    paste("p_bias_period",1:number_of_periods,sep="."),
                    paste("CORR_period",1:number_of_periods,sep=".")
  )
  obj_fun <- data.frame(of = obj_names,
                        basin = rbind(nse_,
                                      kge_,
                                      p_bias_,
                                      corr_,
                                      NSE_period,
                                      KGE_period,
                                      p_bias_period,
                                      CORR_period),
                        row.names = NULL)
  return(obj_fun)
}
#' Plot main objective function values
#'
#' Currently two options for plotting the main objectives are provided by
#' visCOS: Plotting the different objective functions values as a set of
#' bar plots \code{barplot_of} and plotting a summary table in form of
#' a large raster of all the objective function values \code{rasterplot_of}.
#'
#' @name plot_main_of
NULL
#' Bar plot for the Main Objective Function Values
#'
#' @rdname of_overview
#' @export
main_of_barplot <- function(cos_data) {
  main_of_names <- c("NSE","KGE","CORR","p_bias")
  assert_dataframe(cos_data)
  of <- main_of_compute(cos_data)
  # calculate objective functions
  num_basins <- ncol(of) - 1
  # melt the of:
  # a bit of a mess, and there is probably a better solution...
  melted_of <- suppressMessages( reshape2::melt(of) ) %>%
    mutate(of_group = ifelse(of %>% as.character %>% startsWith(.,main_of_names[1]),main_of_names[1],
                             ifelse(of %>% as.character %>% startsWith(.,main_of_names[2]),main_of_names[2],
                                    ifelse(of %>% as.character %>% startsWith(.,main_of_names[3]),main_of_names[3],
                                           ifelse(of %>% as.character %>% startsWith(.,main_of_names[4]),main_of_names[4],NA)))))
  # define plot-list function
  plotlist_fun_barplot <- function(of_name) {
    of_to_plot <- melted_of %>% filter( of_group == of_name)
    if (of_name == "p_bias") {
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
  plot_list <- lapply(main_of_names, plotlist_fun_barplot)
  names(plot_list) <- main_of_names
  return(plot_list)
}
#' Bar plot for the Main Objective Function Values
#'
#' @rdname of_overview
#' @import pasta
#' @export
main_of_rasterplot <- function(cos_data) {
  main_of_names <- c("NSE","KGE","CORR","p_bias")
  regex_main_of <- main_of_names %&% ".*"
  assert_dataframe(cos_data)
  of <- main_of_compute(cos_data)
  #
  plot_list <- lapply(regex_main_of,function(x) plot_fun_raster(x,of)) %>%
    set_names(main_of_names)
  return(plot_list)
}
plot_fun_raster <- function(regex_single_of,of) {
  # function definitions ----------------------------------------------------
  extract_single_of <- function(of){
    idx <- grep(regex_single_of,of$of)
    return(of[idx, ])
  }
  add_facet_info <- function(of) {
    facet_column <- nrow(of) %>% 
      magrittr::subtract(1) %>% 
      rep("period",.) %>% 
      c("overall",.)
    return( cbind(of,facets = facet_column) )
  }
  reverse_basin_levels <- function(plot_data) {
    plot_data$variable <- factor(plot_data$variable,
                                 levels = plot_data$variable %>%
                                   levels() %>%
                                   rev()
                                 )
    return(plot_data)
  }
  reverse_facetting_levels <- function(plot_data) {
    plot_data$facets <- factor(plot_data$facets,
                               levels = plot_data$facets %>%
                                 levels() %>%
                                 rev()
                               )
    return(plot_data)
  }
  bind_and_round_value <- function(of,gglimits,digits) {
    dplyr::mutate(of,
                  value = pmax(value,gglimits[1]) %>% 
                    pmin(.,gglimits[2]) %>% 
                    round(.,digits)
                  )
  }
  # computation -------------------------------------------------------------
  if (regex_single_of == "p_bias.*") {
    # pbias has different limits :(
    gglimits <- c(-viscos_options("of_limits")[2]*100,
                  viscos_options("of_limits")[2]*100)
  } else {
    gglimits <- viscos_options("of_limits")
  }
  #
  plot_data <- of %>%
    extract_single_of() %>%
    add_facet_info() %>%
    reshape2::melt(., id.vars = c("of","facets")) %>%
    reverse_basin_levels() %>%
    reverse_facetting_levels() %>%
    bind_and_round_value(.,gglimits,2)
  # ggplot ------------------------------------------------------------------
  plt_out <- ggplot(plot_data,
                    aes(of,variable, fill = value),
                    environmnet = environment()) +
    geom_raster(position = "identity") +
    coord_fixed(ratio = 5)  +
    facet_grid(~ facets,  scales = "free_x", space = "free") +
    theme( legend.position = "none")  +
    geom_tile(color = "white", size = 0.25 ) +
    geom_text(aes(of,variable, label = as.character(value,2)),
              color = "black")
  return(plt_out)
}
