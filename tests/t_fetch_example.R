# test fetch.example  -----------------------------------------------------
# tests for channel.period function 

# author: Daniel
#  ------------------------------------------------------------------------
library(visCOS)
data <- fetch.runoff_example()

# test if period combination for 1 hyd-year data are ok:
set1 <- seq(1:12)
set2 <- seq(1:12)
# 
for (m in set1) {
  for (n in set2) {
    channeled_data <- channel.periods(data,start_month = set1[m], end_month = set2[n])
    different_periods <- unique(channeled_data$period)
    # 
    stopifnot( different_periods %in% c(0,1) )
  }
}
# test if period combination for 2 hyd-years data are ok:
data2 <- rbind(data,data)
for (m in set1) {
  for (n in set2) {
    channeled_data <- channel.periods(data2,start_month = set1[m], end_month = set2[n])
    different_periods <- unique(channeled_data$period)
    # 
    stopifnot( different_periods %in% c(0,1,2) )
  }
}

