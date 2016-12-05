#' Plot List for Runoff Peaks
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @importFrom tibble tibble
peak_plot <- function(cos_data,
                                 n_events= 10L,
                                 window_size = 24L) {
  # pre:
  assert_dataframe(cos_data)
  n_events_int <- as.integer(n_events)
  window_size_int <- as.integer(window_size)
  if( is.na(n_events_int)  |
      is.nan(n_events_int) |
      is.infinite(n_events_int) |
      !is.integer(n_events_int) ) {
    stop("n_events is ill defined")
  }
  if( is.na(window_size)  |
      is.nan(window_size) |
      is.infinite(window_size) |
      !is.integer(window_size) ) {
    stop("window_size is ill defined")
  }
  data1 <- cos_data %>%
     select( starts_with(viscos_options("name_o")) )
  data2 <- cos_data %>%
     select( starts_with(viscos_options("name_s")) )
  data_numbers <- names(data1) %>%
    gsub(viscos_options("name_o"),"",.,ignore.case = TRUE) %>%
    gsub("\\D","",.,ignore.case = TRUE)
  # make plotlist:
  plotlist <- lapply(1:ncol(data1), function(x) plotlist_one_basin(data1[,x],
                                                                   data2[,x],
                                                                   n_events_int,
                                                                   window_size_int)) %>%
    set_names(.,paste("basin", data_numbers, sep =""))
  return(plotlist)
}
plotlist_one_basin <- function(qobs,qsim,n_events_int,window_size_int) {
  single_data <- tibble::tibble(time = as.integer(1:length(qobs)),
                                obs = as.double(qobs),
                                sim = as.double(qsim))
  #
  peak_idx <- find_peaks(single_data$obs,m = window_size_int)
  peak_organised <- tibble::tibble(idx = as.integer(peak_idx),
                           peak_obs = single_data$obs[peak_idx],
                           peak_sim = single_data$sim[peak_idx])
  highest_peaks_organised <- peak_organised$peak_obs %>%
    sort(decreasing = TRUE) %>%
    .[1:n_events_int] %>%
    '%in%'(peak_organised$peak_obs,.) %>%
    which( . ) %>%
    peak_organised[., ]
  #
  overview_plot <- ggplot() +
    geom_line(data = single_data,aes(x = time, y = sim), col = viscos_options("color_s")) +
    geom_line(data = single_data,aes(x = time, y = obs), col = viscos_options("color_o")) +
    geom_point(data = highest_peaks_organised, aes(idx, peak_obs))
  overview_scatter <- ggplot() +
    geom_point(data = single_data, aes(obs,sim), color = "#DDDDDD") +
    geom_abline() +
    geom_point(data = highest_peaks_organised, aes(peak_obs,peak_sim), size = 4) +
    expand_limits(x = 0, y = 0)
  sub_plots <- lapply(1:nrow(highest_peaks_organised),
                      function(x) sub_peakplot_fun(x,window_size_int,highest_peaks_organised,single_data) ) %>%
    set_names(.,paste("event_plot",1:length(.),sep=""))
  return(overview = append(list(overview = overview_plot,scatter = overview_scatter), sub_plots))
}
  ####
  # peak finder function:
  find_peaks <- function (x, m = 3){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
      z <- i - m + 1
      z <- ifelse(z > 0, z, 1)
      w <- i + m + 1
      w <- ifelse(w < length(x), w, length(x))
      if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
    pks <- unlist(pks)
    pks
  }
  ####
  # sub plot function:
  sub_peakplot_fun <- function(x, window_size, highest_peaks_organised, peak_data) {
    point <- highest_peaks_organised[x,]
    plot_sub <- ggplot() +
      geom_line(data = peak_data[(point$idx - window_size):(point$idx + window_size),],
                aes(x = time, y = sim),
                col = "orange") +
      geom_line(data = peak_data[(point$idx - window_size):(point$idx + window_size),],
                aes(x = time, y = obs),
                col = "steelblue") +
      geom_point(data = point, aes(idx, peak_obs))
    return(plot_sub)
  }
