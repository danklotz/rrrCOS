# test fetch.example  -----------------------------------------------------
# This script tests if fetch.runoff_example() works properly. 
# It also serves as an example for how simple tests can be done. Note: 
# For more complex tests you might want to use the test_that package from 
# Hadley Wickham

# author: Daniel
#  ------------------------------------------------------------------------

library(visCOS)
data <- fetch.runoff_example()
test1 <- dim(data) == c(9000,11)
test2 <- names(data) == c("yyyy","mm","dd","hh","min","QOBS_0001","QOSI_0001","QSIM_0001","QOBS_0002","QOSI_0002","QSIM_0002")

stopifnot(min(c(test1,test2)) == 1)
