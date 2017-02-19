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

# setup: 
of <- hydroGOF::NSE 
lb <- 0.5
window_size = 400
steps <- 3

# experimental data: 
cos_data <- get_viscos_example( ) %>% 
  remove_junk(.) %>% 
  mark_periods(.)

# sketch 4 the future windowing function:
    of_windowing <- function(cos_data,
                             of, 
                             lb = 0.0, 
                             window_size = 500, 
                             steps = 5, 
                             na_filling = FALSE) {
      length_data <- nrow(cos_data)
      # in future we need to quarantee that step == 0 and window_size == 0 will 
      #   not make problems!
      all_window_sizes <- seq(from = window_size,
                              to = length_data, 
                              length.out = steps) %>% 
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
      apply_lb <- function(in_data,lb) {
        pmax(in_data,lb)
      }
      #
      window_data <- sapply(all_window_sizes,roll_of) %>%
        apply(.,2,fill_na_up) %>%
        apply(.,2,low_bind,lb) %>% 
        as_tibble(.) %>% 
        set_names("of_window" %&% sprintf("%.3i",1:length(all_window_sizes))) %>% 
        dplyr::mutate(idx = 1:length_data,
                      qobs = cos_data$QOBS_0002, 
                      qsim = cos_data$QSIM_0002)
      gathered_window <- gather(window_data, key, value, -idx) %>% 
        mutate(le_group = ifelse((key == "qobs" | key == "qsim"),"q","of"))
      ggplot(gathered_window, aes(x = idx)) + 
        facet_wrap(~le_group, scales = "free_y", ncol = 1) +
        geom_raster(data = subset(gathered_window, le_group == "of"), aes(x = idx, y = key, fill = value)) +
        geom_line(data = subset(gathered_window, le_group == "q"), aes(x = idx, y = value, color = key))
    }




