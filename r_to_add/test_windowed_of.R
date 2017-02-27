# test for a posisble functionaliy for windowed objective functions 
# author Klotz (02/2017)

# pre sets:
require(visCOS)
require(dplyr)
require(ggplot2)
require(magrittr)
require(purrr)
require(hydroGOF)
require(tidyr)
require(pasta)
# probably need it :( - but maybe I can stal it or something.. mh 
# solution notes: 
#   - the RcppRoll is fast but not realy flexible, not sure if I can use it
#   - gplotslooks nice form the funcitonality, but the code is strange... 
# require(RcppRoll)
require(gplots)

# experimental data: 
cos_data <- get_viscos_example( ) %>% 
  remove_junk(.) %>% 
  mark_periods(.)

# sketch 4 the future windowing function:
  of_windowing <- function(cos_data,
                           of, 
                           lb = 0.0, 
                           min_window_size = 500,
                           max_window_size = 1000,
                           number_windows = 3, 
                           na_filling = FALSE) {
    length_data <- nrow(cos_data)
    # in future we need to quarantee that step == 0 and window_size == 0 will 
    #   not make problems!
    all_window_sizes <- seq(from = window_size,
                            to = max_window_size, 
                            length.out = number_windows) %>% 
      as.integer(.) 
    # all window sizes must be uneven!
    # all_window_sizes[mod(all_window_sizes,2) == 0] <- 
    #   all_window_sizes[mod(all_window_sizes,2) == 0] -1
    # calc g's
    roll_of <- function(window_size) {
      gtools::running(X = cos_data$QSIM_0002, 
                      Y = cos_data$QOBS_0002,
                      fun = of,
                      width = window_size,
                      pad = TRUE,
                      align = "right")
    }
    fill_na_up <- function(in_data) {
      if (na_filling) {
        fill_idx <- in_data %>% 
          is.na(.) %>% 
          not(.) %>% 
          which(.) %>% 
          min(.)
        in_data[1:fill_idx] <- in_data[fill_idx]
      }
      return(in_data)
    }
    confine  <- function(in_data) {
      pmax(in_data,lb)
    }
    #
    window_data <- sapply(all_window_sizes,roll_of) %>%
      apply(., 2, fill_na_up) %>%
      apply(., 2, confine) %>% 
      as_tibble(.) %>% 
      set_names("of_window" %&% sprintf("%.3i",1:length(all_window_sizes))) %>% 
      dplyr::mutate(idx = 1:length_data,
                    posixdate = cos_data[["posixdate"]],
                    qobs = cos_data[["QOBS_0002"]], 
                    qsim = cos_data[["QSIM_0002"]])
    gathered_window <- gather(window_data, key, value, -idx, -posixdate) %>% 
      mutate(le_group = ifelse((key == "qobs" | key == "qsim"),"q-basin_2","of-basin_2"))
  }

  # test function 
  of <- hydroGOF::NSE 
  lb <- 0.5
  steps <- 3
  windowed_data <- of_windowing(cos_data, 
                                of = of, 
                                lb = lb, 
                                min_window_size = 1000,
                                max_window_size = 10000,
                                number_windows = steps)
  
  ggplot(windowed_data, aes(x = idx)) + 
    facet_wrap(~le_group, scales = "free_y", ncol = 1) +
    geom_raster(data = subset(windowed_data, le_group == "of"), aes(x = posixdate, y = key, fill = value)) +
    geom_line(data = subset(windowed_data, le_group == "q"), aes(x = posixdate, y = value, color = key))


