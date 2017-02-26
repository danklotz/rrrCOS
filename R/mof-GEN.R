# ---------------------------------------------------------------------------
# Code for the Main Objective Functions (main_of)
# authors: Daniel Klotz, Johannes Wesemann, Mathew Herrnegger
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
#' @import hydroGOF
#' @import dplyr
#'
#' @export
main_of_compute <- function(cos_data) {
  # def: ====================================================================
  assert_dataframe(cos_data)
  name_o <- viscos_options("name_o")
  name_s <- viscos_options("name_s")
  name_period <- viscos_options("name_COSperiod")
  if (!exists(name_period, where = cos_data)) {
    stop("Error! Period-Column missing in cos_data, use `mark_periods`")
  }
  evaluation_data <- cos_data[cos_data[[name_period]] > 0, ]
  number_of_basins <- evaluation_data %>%
    names() %>%
    unique() %>%
    grepl(name_o, ., ignore.case = TRUE) %>%
    sum()
  data_periods <- evaluation_data %>%
    magrittr::extract2(name_period) %>%
    unique()
  number_of_periods <- data_periods %>% length
  # compute main-of for entire data: ========================================
  o_pick <- dplyr::select(evaluation_data,starts_with(name_o)) %>% unname()
  s_pick <- dplyr::select(evaluation_data,starts_with(name_s)) %>% unname()
  nse_all <- hydroGOF::NSE(s_pick,o_pick)
  kge_all <- hydroGOF::KGE(s_pick,o_pick)
  p_bias_all <- hydroGOF::pbias(s_pick,o_pick)
  corr_all <- cor(s_pick,o_pick) %>% diag()
  # compute periodwise main-of: =============================================
  # pre allocations: ########################################################
  NSE_period <- matrix(nrow = number_of_periods,
                       ncol = as.integer(number_of_basins),
                       data = NA)
  KGE_period <- NSE_period
  p_bias_period <- NSE_period
  CORR_period <- NSE_period
  # calculation loop, proabbly slow :( ######################################
  for (k in 1:number_of_periods) {
    o_pick <- dplyr::filter(evaluation_data,period == data_periods[k]) %>%
      dplyr::select(starts_with(name_o)) %>%
      unname()
    s_pick <- dplyr::filter(evaluation_data,period == data_periods[k]) %>%
      dplyr::select(starts_with(name_s)) %>%
      unname()
    NSE_period[k,1:number_of_basins] <- hydroGOF::NSE(s_pick,o_pick)
    KGE_period[k,1:number_of_basins] <- hydroGOF::KGE(s_pick,o_pick)
    p_bias_period[k,1:number_of_basins] <- hydroGOF::pbias(s_pick,o_pick)
    CORR_period[k,1:number_of_basins] <- cor(s_pick,o_pick) %>% diag(.)
  }
  # clean up: ===============================================================
  obj_names <- c("NSE","KGE","p_bias","CORR",
                 paste("NSE_period",1:number_of_periods,sep = "."),
                 paste("KGE_period",1:number_of_periods,sep = "."),
                 paste("p_bias_period",1:number_of_periods,sep = "."),
                 paste("CORR_period",1:number_of_periods,sep = ".")
                 )
  obj_fun <- data.frame(of = obj_names,
                        basin = rbind(nse_all,
                                      kge_all,
                                      p_bias_all,
                                      corr_all,
                                      NSE_period,
                                      KGE_period,
                                      p_bias_period,
                                      CORR_period),
                        row.names = NULL)
  return(obj_fun)
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
#' @export
main_of_barplot <- function(cos_data) {
  # def: ====================================================================
  assert_dataframe(cos_data)
  # functions: ==============================================================
  assign_ofgroups <- function(of_melted,mof_names) {
    of_string <- as.character(of_melted$of)
    of_melted$of_group <- of_string %>% 
      replace(.,startsWith(of_string,mof_names[1]),mof_names[1]) %>% 
      replace(.,startsWith(of_string,mof_names[2]),mof_names[2]) %>% 
      replace(.,startsWith(of_string,mof_names[3]),mof_names[3]) %>% 
      replace(.,startsWith(of_string,mof_names[4]),mof_names[4])
    return(of_melted)
  }
  # plot-list function:
  barplot_fun <- function(of_name,of_melted) {
    of_to_plot <- of_melted %>% filter( of_group == of_name)
    if (of_name == "p_bias") {
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
  mof_names <- c("NSE","KGE","CORR","p_bias")
  of <- main_of_compute(cos_data)
  num_basins <- ncol(of) - 1
  of_melted <- suppressMessages( reshape2::melt(of) ) %>%
    assign_ofgroups(.,mof_names)
  # plotting ================================================================
  plot_list <- lapply(mof_names, function(x) barplot_fun(x,of_melted)) %>% 
    magrittr::set_names(mof_names)
  return(plot_list)
}

#' Bar plot for the Main Objective Function Values
#'
#' @rdname of_overview
#' @import pasta
#' @export
main_of_rasterplot <- function(cos_data) {
  mof_names <- c("NSE","KGE","CORR","p_bias")
  regex_main_of <- mof_names %.% "*"
  assert_dataframe(cos_data)
  of <- main_of_compute(cos_data)
  #
  plot_list <- lapply(regex_main_of,function(x) plot_fun_raster(x,of)) %>%
    set_names(mof_names)
  return(plot_list)
}

# plot function -------------------------------------------------------------
plot_fun_raster <- function(regex_single_of,of) {
  # function definitions ====================================================
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
  prepared_data <- of %>%
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
    geom_text(aes(of,variable, label = as.character(value,2)),
              color = "black")
  return(plt_out)
}

