#' order names of runoff_data 
#'
#' xxx
#' @export
prepare_names <- function(runoff_data) {
  # def: 
    require(magrittr, quietly = TRUE)
    assert_chunk(runoff_data)
  # calc:
  runoff_names <- names(runoff_data) %>% gsub("\\d","",.) 
  # get numbers and remove leading zeros
  runoff_nums <- names(runoff_data) %>% 
    gsub("\\D","",.) %>%  
    as.numeric %>%  
    as.character
  runoff_nums[is.na(runoff_nums)] = ""
  # paste new nums as new data_names 
  names(runoff_data) <- paste(runoff_names, runoff_nums, sep = "")
  return(runoff_data)
}