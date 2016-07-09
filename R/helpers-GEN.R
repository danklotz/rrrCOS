get_regex_for_runoff_data <- function() {
  regex_pattern <- paste("^",viscos_options()$name_COSyear,"$|", 
                         "^",viscos_options()$name_COSmonth,"$|",
                         "^",viscos_options()$name_COSday,"$|",
                         "^",viscos_options()$name_COShour,"$|",
                         "^",viscos_options()$name_COSmin,"$|",
                         viscos_options()$name_COSobs,".*|",
                         viscos_options()$name_COSsim,".*|",
                         viscos_options()$name_COSposix,"|",
                         viscos_options()$name_COSperiod,
                         sep = "")
  return(regex_pattern)
}
