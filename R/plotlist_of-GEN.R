#' plotlists for the objective functions 
#'
#' To be written soon
#' @export
make_plotlist <- function(runoff_data, 
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
  names(of) <- tolower(names(of))
  num_basins <- get_basin_numbers(runoff_data)
  plot_titles <- paste(viscos_options("plot_title"),num_basins,sep = "")
  user_of <- of[[tolower(of_name)]] 
  only_one_period_or_total <- is.null(dim(user_of)[1])
  # (I)
  if ( only_one_period_or_total ) {
    user_of %<>% 
      t() %>%
      pmax(.,viscos_options("limits")[1]) %>%
      as.data.frame()
    names(user_of) <- plot_titles
  } else {
    user_of %<>%
      pmax(.,viscos_options("limits")[1]) %>%
      as.data.frame()
    names(user_of) <- plot_titles
  }
  # (II)
  if ( only_one_period_or_total ) {
    user_of$periods <- 1
  } else {
    user_of$periods <- unique(runoff_data$period) %>% 
      extract(. > 0)
  }
  # define plot function: 
  plot_function <- function(k) {
    ggplot(data = user_of,environmnet = environment()) +
    geom_bar(stat = "identity",
             position = "identity",
             aes_string(x = "periods", y = plot_titles[k],fill = plot_titles[k])) +
    theme_light(base_size = 15) +
    ggtitle(plot_titles[k]) +
    xlab(viscos_options("xlab")) +
    ylab( gsub("_.*","", of_name) ) + # remove everything after "_" 
    scale_y_continuous(limits = viscos_options("limits")) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 50, hjust = 1)
          ) + 
    scale_fill_gradient2(space = "Lab",
                         low = viscos_options("color_of_low"),
                         mid = viscos_options("color_of_mid"),
                         high = viscos_options("color_of_high"),
                         midpoint = viscos_options("midpoint"),
                         limits = viscos_options("limits") )
  }
  # apply plot function over all columns in user_of
  eval_size <- ncol(user_of)
  list_of_barplots <- lapply(1:eval_size,
                    function(k) plot_function(k)
                    )
  return(list_of_barplots)
}

