#' use max as boundary on data 
cut.lowerbound <- function(data,boundary) {data[data < boundary] <- boundary; return(data)}

