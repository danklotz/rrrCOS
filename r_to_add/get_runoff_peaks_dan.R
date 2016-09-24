require("dplyr")
require("ggplot2")
require("magrittr")

#### settings 
# number of events :
n_events <- 10
window_size <- 24


#### data:
runoff_example <- visCOS::get_runoff_example()


#### functions 
sub_plot_fun <- function(x,window_size,highest_peaks_organised,peak_data) {
  point <- highest_peaks_organised[x,]
  plot_sub <- ggplot() +
    geom_line(data = peak_data[(point$idx - window_size):(point$idx + window_size),],
              aes(x = time, y = sim), 
              col = "orange") + 
    geom_line(data = peak_data[(point$idx - window_size):(point$idx + window_size),],
              aes(x = time, y = obs), 
              col = "steelblue") + 
    geom_point(data = point, aes(idx, peak), col = "green")
  return(plot_sub)
}
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


#### calc:
single_data <- tibble(time = as.integer(1:nrow(runoff_example)), 
                      obs = as.double(runoff_example$QOBS_0001), 
                      sim = as.double(runoff_example$QSIM_0001))
#
peak_idx <- find_peaks(single_data$obs,m = window_size)
peak_organised <- tibble(idx = as.integer(peak_idx), 
                         peak = single_data$obs[peak_idx])
highest_peaks_organised <- peak_organised$peak %>% 
  sort(decreasing = TRUE) %>% 
  .[1:n_events] %>%
  '%in%'(peak_organised$peak,.) %>% 
  which( . ) %>% 
  peak_organised[., ]
#
overview_plot <- ggplot() +
  geom_line(data = single_data,aes(x = time, y = sim), col = "orange") + 
  geom_line(data = single_data,aes(x = time, y = obs), col = "steelblue") + 
  geom_point(data = highest_peaks_organised, aes(idx, peak))
# 
sub_plots <- lapply(1:nrow(highest_peaks_organised),
                    function(x) sub_plot_fun(x,window_size,highest_peaks_organised,single_data) )
# subplot function 

