# test if data is a data frame
# 
# returns error message if the input: "data" is not of class "data.frame" 
assert.dataframe <- function(data) {
  if ( !is.data.frame(data) ) stop("runoff_data is no data_frame!")
}