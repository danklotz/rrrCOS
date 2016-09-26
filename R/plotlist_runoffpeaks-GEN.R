#' Plot List for Runoff Peaks 
#' @export
plotlist_runoffpeaks <- function(runoff_data,
                                 n_events = 10,
                                 window_size = 24) {
  require("ggplot2", quietly = TRUE)
  require("magrittr", quietly = TRUE)
  #
  data1 <- runoff_data %>%
    dplyr::select(.,dplyr::starts_with(viscos_options("name_data1")))
  data2 <- runoff_data %>%
    dplyr::select(.,dplyr::starts_with(viscos_options("name_data2")))
  data_numbers <- names(data1) %>%
    gsub(viscos_options("name_data1"),"",.,ignore.case = TRUE) %>% 
    gsub("\\D","",.,ignore.case = TRUE)
  plotlist <- lapply(1:ncol(data1), function(x) plotlist_one_basin(data1[,x],
                                                                   data2[,x],
                                                                   n_events, 
                                                                   window_size)) %>%
    rename_peakslist(.,data_numbers)
  return(plotlist)
}
plotlist_one_basin <- function(qobs,qsim,n_events,window_size) {
   #### calc:
  single_data <- tibble::tibble(time = as.integer(1:length(qobs)),
                                obs = as.double(qobs),
                                sim = as.double(qsim))
  #
  peak_idx <- find_peaks(single_data$obs,m = window_size)
  peak_organised <- tibble::tibble(idx = as.integer(peak_idx), 
                           peak_obs = single_data$obs[peak_idx], 
                           peak_sim = single_data$sim[peak_idx])
  highest_peaks_organised <- peak_organised$peak_obs %>% 
    sort(decreasing = TRUE) %>% 
    .[1:n_events] %>%
    '%in%'(peak_organised$peak_obs,.) %>% 
    which( . ) %>% 
    peak_organised[., ]
  #
  overview_plot <- ggplot() +
    geom_line(data = single_data,aes(x = time, y = sim), col = viscos_options("color_data2")) + 
    geom_line(data = single_data,aes(x = time, y = obs), col = viscos_options("color_data1")) + 
    geom_point(data = highest_peaks_organised, aes(idx, peak_obs))
  overview_scatter <- ggplot(highest_peaks_organised) + 
    geom_abline(color = viscos_options("color_of_mid")) +
    geom_point(aes(peak_obs,peak_sim), size = 4) +
    expand_limits(x = 0, y = 0)
  sub_plots <- lapply(1:nrow(highest_peaks_organised),
                      function(x) sub_peakplot_fun(x,window_size,highest_peaks_organised,single_data) )
  return(overview = append(list(overview = overview_plot,scatter = overview_scatter), sub_plots))
}
# helper function to give the list some names
rename_peakslist <- function(data_list,data_numbers) {
  d_length <- length(data_list)
  d_names <- paste("basin", data_numbers, sep ="")
  names(data_list) <- d_names
  return(data_list)
}
  ####
  # peak finder function 
    #   propsoed by http://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
  #   btw. also a nice idea: http://stats.stackexchange.com/questions/36309/how-do-i-find-peaks-in-a-dataset
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
  # sub plot function
  sub_peakplot_fun <- function(x,window_size,highest_peaks_organised,peak_data) {
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
