# ---------------------------------------------------------------------------
# Code for the Main Objective Functions (main_of)
# authors: Daniel Klotz, Johannes Wesemann, Mathew Herrnegger
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#' Objective Functions Wrap 
#'
#' @import pasta
#' @import lazyeval 
#' @export
of <- function(cos_data,
               d_metrics = list(nse=d_nse,kge=d_kge,pbias=d_pbias,corr=d_cor),
               type = "compute",
               ...) {
  if(class(type) != "character") stop("Input argument `type` must be a character")
  le_dots <- lazy_dots(...)
  # switch:
  if (type == "compute") {
    of_compute(cos_data, d_metrics)
  } else if (type == "barplot") {
    of_barplot(cos_data, d_metrics)
  } else if (type == "rasterplot") {
    of_rasterplot(cos_data, d_metrics)
  } else if (type == "explore") {
    of_explore(cos_data, d_metrics)
  } else if (type == "compare") {
    start_date <- NULL
    end_date <- NULL
    cos_data2 <- NULL
    if ("cos_data2" %in% names(le_dots)) {
      cos_data2 <- lazy_eval(le_dots$cos_data2)
    } 
    of_compare(d1 = cos_data,
               d2 = cos_data2)
  } else {
    stop("There is no `type` called" %&&% type)
  }
}
# ---------------------------------------------------------------------------
#' Get basic objective function for cos_data
#'
#' Calculate basic objective functions(NSE, KGE, percentage BIAS, Correlation)
#' for every basin and the chosen periods.
#'
#' @param cos_data cos_data data.frame.
#' @return list of basic objective function evaluated for the different
#' hydrological years and over the whole timespan.
#'
#' @import pasta
#' @import coscos
#' @import tibble
#' @importFrom dplyr select filter starts_with 
#' @importFrom magrittr set_names
#'
#' @export
of_compute <- function(cosdata, 
                       d_metrics = list(nse = d_nse, 
                                        kge = d_kge, 
                                        pbias = d_pbias, 
                                        corr = d_cor)) {
  # pre: ====================================================================
  cos_data <- coscos::check_cosdata(cosdata)
  if(!(class(d_metrics) == "list")) {
    d_metrics <- list(d_metrics)
  }
  if (is.null(names(d_metrics))){
    d_names <- "d" %&% 1:length(d_metrics)
  } else {
    d_names <- names(d_metrics)
  }
  #
  evaluation_data <- cos_data[cos_data[[name_period]] > 0, ]
  number_of_basins <- evaluation_data %>%
    names(.) %>%
    unique(.) %>%
    grepl(name_o, ., ignore.case = TRUE) %>%
    sum(.)
  data_periods <- evaluation_data %>%
    .[[name_period]] %>%
    unique(.)
  number_of_periods <- data_periods %>% length

  # compute main-of for entire data: ========================================
  d_mean <- sapply(d_metrics, function(of_, x, y) as.numeric(of_(x,y)),
                 x = dplyr::select(evaluation_data,dplyr::starts_with(name_o)) %>% unname(.),
                 y = dplyr::select(evaluation_data,dplyr::starts_with(name_s)) %>% unname(.) ) %>% 
    t(.) %>% 
    as_tibble(.) %>% 
    magrittr::set_names(., "basin" %_% 1:number_of_basins) %>% 
    cbind.data.frame(of = d_names,.) 
  # compute periodwise main-of: =============================================
  period_compute <- function(k) {
    o_pick <- dplyr::filter(evaluation_data,period == data_periods[k]) %>%
      dplyr::select(.,starts_with(name_o)) %>%
      unname(.)
    s_pick <- dplyr::filter(evaluation_data,period == data_periods[k]) %>%
      dplyr::select(.,starts_with(name_s)) %>%
      unname(.)
    d_measures <- t(sapply(d_metrics, function(of_,x,y) as.numeric(of_(x,y)),
                 x = o_pick,
                 y = s_pick ))
    return(d_measures)
  }
  d_names_periods <- d_names %_% "period" %.% rep(1:number_of_periods, each = length(d_names))
  d_periods <- lapply(1:number_of_periods, period_compute) %>%
    do.call(rbind,.) %>% 
    as_tibble(.) %>% 
    magrittr::set_names("basin" %_% 1:number_of_basins) %>% 
    cbind(of = d_names_periods,.) %>% 
    .[order(.$of), ]
  #
  of_all <- rbind(d_mean,d_periods) %>% 
    as_tibble(.)
  of_all$of <- as.character(of_all$of)
  return(of_all)
}

# ---------------------------------------------------------------------------
#' Plot main objective function values
#'
#' Currently two options for plotting the main objectives are provided by
#' visCOS: Plotting the different objective functions values as a set of
#' bar plots \code{barplot_of} and plotting a summary table in form of
#' a large raster of all the objective function values \code{rasterplot_of}.
#'
#' @name plot_main_of
NULL

# ---------------------------------------------------------------------------
#' Bar plot for the Main Objective Function Values
#'
#' @rdname of_overview
#'
#' @import coscos
#' 
#' @export
of_barplot <- function(cosdata, d_metrics = list(nse = d_nse, 
                                        kge = d_kge, 
                                        pbias = d_pbias, 
                                        corr = d_cor)) {
  # pre: ====================================================================
  cos_data <- coscos::check_cosdata(cosdata)
  if(!(class(d_metrics) == "list")) {
    d_metrics <- list(d_metrics)
  }
  if (is.null(names(d_metrics))){
    d_names <- "d" %&% 1:length(d_metrics)
  } else {
    d_names <- names(d_metrics)
  }
  # functions: ==============================================================
  assign_ofgroups <- function(of_melted,of_names) {
    of_string <- as.character(of_melted$of)
    of_melted$of_group <- of_string %>% 
      replace(., startsWith(of_string,of_names[1]), of_names[1]) %>% 
      replace(., startsWith(of_string,of_names[2]), of_names[2]) %>% 
      replace(., startsWith(of_string,of_names[3]), of_names[3]) %>% 
      replace(., startsWith(of_string,of_names[4]), of_names[4])
    return(of_melted)
  }
  # plot-list function:
  barplot_fun <- function(of_name,of_melted) {
    of_to_plot <- of_melted %>% filter( of_group == of_name)
    # bad solution, but will be fine for now? 
    if (of_name == "pbias") {
      gglimits <- c(-viscos_options("of_limits")[2]*100,
                   viscos_options("of_limits")[2]*100)
    } else {
      gglimits <- viscos_options("of_limits")
    }
    plt_out <- ggplot(data = of_to_plot) +
      geom_bar(stat = "identity",
               position = "identity",
               aes(x = of, y = value, fill = value)) +
      facet_wrap(~ variable, ncol = 1) +
      ggtitle(of_name) +
      ylim(gglimits)
    return(plt_out)
  }
  # computations: ===========================================================
  of_data <- of_compute(cos_data,d_metrics)
  num_basins <- ncol(of_data) - 1
  of_melted <- suppressMessages( reshape2::melt(of_data) ) %>%
    assign_ofgroups(.,d_names)
  # plotting ================================================================
  plot_list <- lapply(d_names, function(x) barplot_fun(x,of_melted)) %>% 
    magrittr::set_names(d_names)
  return(plot_list)
}

#' Bar plot for the Main Objective Function Values
#'
#' @rdname of_overview
#' @import pasta
#' @export
of_rasterplot <- function(cos_data, d_metrics = list(nse = d_nse, 
                                        kge = d_kge, 
                                        pbias = d_pbias, 
                                        corr = d_cor)) {
  # def: ====================================================================
  build_tibble(cos_data)
  if(!(class(d_metrics) == "list")) {
    d_metrics <- list(d_metrics)
  }
  if (is.null(names(d_metrics))){
    d_names <- "d" %&% 1:length(d_metrics)
  } else {
    d_names <- names(d_metrics)
  }
  # computations: ===========================================================
  regex_main_of <- d_names %.% "*"
  of_data <- of_compute(cos_data, d_metrics)
  #
  plot_list <- lapply(regex_main_of,function(x) plot_fun_raster(x,of_data)) %>%
    magrittr::set_names(d_names)
  return(plot_list)
}

# plot function -------------------------------------------------------------
plot_fun_raster <- function(regex_single_of,of_data) {
  # function definitions ====================================================
  extract_single_of <- function(of_data){
    idx <- grep(regex_single_of,of_data$of)
    return(of_data[idx, ])
  }
  add_facet_info <- function(of_data) {
    facet_column <- nrow(of_data) %>% 
      magrittr::subtract(1) %>% 
      rep("period",.) %>% 
      c("overall",.)
    return( cbind(of_data,facets = facet_column) )
  }
  reverse_basin_levels <- function(prepared_data) {
    prepared_data$variable <- factor(prepared_data$variable,
                                 levels = prepared_data$variable %>%
                                   levels() %>%
                                   rev()
                                 )
    return(prepared_data)
  }
  reverse_facetting_levels <- function(prepared_data) {
    prepared_data$facets <- factor(prepared_data$facets,
                               levels = prepared_data$facets %>%
                                 levels() %>%
                                 rev()
                               )
    return(prepared_data)
  }
  bind_and_round_value <- function(of,gglimits,digits) {
    dplyr::mutate(of,
                  value = pmax(value,gglimits[1]) %>% 
                    pmin(.,gglimits[2]) %>% 
                    round(.,digits)
                  )
  }
  # computation =============================================================
  if (regex_single_of == "p_bias.*") {
    # pbias has different limits :(
    gglimits <- c(-viscos_options("of_limits")[2]*100,
                  viscos_options("of_limits")[2]*100)
  } else {
    gglimits <- viscos_options("of_limits")
  }
  #
  prepared_data <- of_data %>%
    extract_single_of() %>%
    add_facet_info() %>%
    reshape2::melt(., id.vars = c("of","facets")) %>%
    reverse_basin_levels() %>%
    reverse_facetting_levels() %>%
    bind_and_round_value(.,gglimits,2)
  # ggplot ==================================================================
  plt_out <- ggplot(prepared_data,
                    aes(of,variable, fill = value),
                    environmnet = environment()) +
    geom_raster(position = "identity") +
    coord_fixed(ratio = 5)  +
    facet_grid(~ facets,  scales = "free_x", space = "free") +
    theme( legend.position = "none")  +
    geom_tile(color = "white", size = 0.25 ) +
    geom_text(aes(of,variable, label = as.character(sprintf("%0.2f",value,2))),
              color = "black")
  return(plt_out)
}

