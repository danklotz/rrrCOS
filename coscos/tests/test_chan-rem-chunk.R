# test: channel.remove_chunk ----------------------------------------------
# Testing if the function channel.remove chunk and its wrap into channel()
# work correctly 

# author: Daniel
#  ------------------------------------------------------------------------
library(visCOS)
data <- fetch.runoff_example()

# test if QOSI is removed correctly!
names_before <- names(data)
names_after <- names( channel.remove_chunk(data) )

stopifnot(length(names_before) > length(names_after))
stopifnot("QOSI_0001" %in% names_before)
stopifnot(!"QOSI_0001" %in% names_after)

# test if wrapping works: 
data_a <- channel.remove_chunk(data)
data_b <-channel(this = remove_chunk, from_that = data)
stopifnot(min(data_a == data_b) == 1)


