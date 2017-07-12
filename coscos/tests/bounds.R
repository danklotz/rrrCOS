# test fetch.example  ------------------------------------------------------
# tests for channel.period function 
# author: Daniel
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# pre: ---------------------------------------------------------------------
library(visCOS)
library(magrittr)
library(ggplot2)

# data prep: ---------------------------------------------------------------
cos_data <- get_viscos_example() %>% 
  remove_junk(.) %>% 
  cook_dates(.)
# generate bounds: =========================================================
# cos_data$QOBS_0001 is very noise so we might want to smoooth it,
# e.g. with loess. 
# We can then use it as our simulation and generate bounds for it!
loess_model <- tibble(idx = 1:nrow(cos_data), 
                          value = cos_data$QOBS_0001) %>% 
  loess(data = ., formula = value ~ idx,span = 0.1) 
loess_data <- tibble(
  yyyy = cos_data$yyyy,
  mm = cos_data$mm, 
  dd = cos_data$dd, 
  hh = cos_data$hh, 
  min = cos_data$min,
  qobs_01 = cos_data$QOBS_0001,
  qsim_01 = predict(loess_model,1:nrow(cos_data)), 
  error = qsim_01 - cos_data$QOBS_0001, 
  lb_01 = qsim_01 - sd(error), 
  ub_01 = qsim_01 + sd(error), 
  posixdate = cos_data$posixdate
)

# plot 4 fun: ==============================================================
ggplot(loess_data) + 
  geom_line(aes(x = posixdate, y = qobs_01), colour = "dodgerblue") +
  geom_ribbon(aes(x = posixdate, ymin = lb_01, ymax = ub_01), 
              fill = "orange", 
              alpha = 0.5) + 
  geom_line(aes(x = posixdate, y = qsim_01), colour = "orange", size = 1) 

# tests --------------------------------------------------------------------
# 1. test if remove_junk works on loess_data ===============================
clean_loess_data <- remove_junk(loess_data) %>% 
  cook_dates(.) %>% 
  mark_periods(.)
needed_names <- c("yyyy",
                  "mm",
                  "dd",
                  "hh",
                  "min",
                  "qobs_01",
                  "qsim_01",
                  "lb_01",
                  "ub_01",
                  "posixdate", 
                  "period")
test <- all( names(clean_loess_data) == needed_names )
if (test) {
  print( test )
} else {
  stop("junk removing did not work properly!")
}

# 2. test if remove_junk works on loess_data ===============================
# 2- test visually if explore_cos_data works with loess & cos_data =========
explore_cos_data(loess_data)
