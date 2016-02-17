# test if everything is OK with the basic objective function list
# 
# returns error messages if the basicOF object is not a list or 
# does not contain all the basic OFs or 
# if it contains more then the basic OFs
assert.basicOF <- function(basicOF) {
  if ( !is.list(basicOF) ) stop("The basic objective funcitons are not stored in a list!")
  assertRightOF <- any( grepl("NSE.*|KGE.*|pBIAS.*|CORR.*",names(basicOF)) == TRUE)
  if ( !assertRightOF ) stop("The basic objective function do not contain the right objective functions (see: xxx)") 
}