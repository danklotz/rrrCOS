get_regex_for_cos_data <- function() {
  regex_pattern <- paste("^",viscos_options("name_COSyear"),"$|",
                         "^",viscos_options("name_COSmonth"),"$|",
                         "^",viscos_options("name_COSday"),"$|",
                         "^",viscos_options("name_COShour"),"$|",
                         "^",viscos_options("name_COSmin"),"$|",
                         viscos_options("name_o"),".*|",
                         viscos_options("name_s"),".*|",
                         viscos_options("name_COSposix"),"|",
                         viscos_options("name_COSperiod"),
                         sep = "")
  return(regex_pattern)
}
get_basin_numbers <- function(cos_data) {
  require("magrittr", quietly = TRUE)
  assert_dataframe(cos_data)
  assert_junk(cos_data)
  #
  d_names <- names(cos_data)
  d_nums <- d_names  %>% gsub('\\D','',.) %>% unique
  d_nums <- d_nums[!(d_nums == "")] %>% as.integer
  return(d_nums)
}
