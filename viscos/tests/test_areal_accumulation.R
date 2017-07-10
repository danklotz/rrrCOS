# 3. test for areal accumulation  -----------------------------------------
## test 1
nb <- c(1,2,3,4,5,6,7,8,9,10,11,12)
to_nb <- c(2,3,4,9,6,9,9,9,12,12,12,0) 
area <- c(44.2,47.6,28.4,11.1,34.8,5.9,87.1,67.1,49.1,58.8,62.9,154.2)
sum_area.real <- c(44.2,91.9,120.3,131.3,34.8,40.7,87.1,67.1,375.3,58.8,62.9,651.3)
test_case <- data.frame(nb,to_nb,area)
#
test_case %<>% cum_area
#
test <- equals(sum_area.real,test_case$cum_area) # weak, because of rounding errors!
stopifnot(min(test) == 1)
## test2 
test_case2 <- read.csv("in/ezfl_links.txt", header = TRUE, sep = ";")
test_case2  %<>% cum_area
# cannot be made properly because johannes-data is wrong :(