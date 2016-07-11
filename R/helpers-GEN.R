get_regex_for_runoff_data <- function() {
  regex_pattern <- paste("^",viscos_options()$name_COSyear,"$|", 
                         "^",viscos_options()$name_COSmonth,"$|",
                         "^",viscos_options()$name_COSday,"$|",
                         "^",viscos_options()$name_COShour,"$|",
                         "^",viscos_options()$name_COSmin,"$|",
                         viscos_options()$name_data1,".*|",
                         viscos_options()$name_data2,".*|",
                         viscos_options()$name_COSposix,"|",
                         viscos_options()$name_COSperiod,
                         sep = "")
  return(regex_pattern)
}
get_basin_numbers <- function(runoff_data) {
  require("magrittr", quietly = TRUE)
  assert_dataframe(runoff_data)
  assert_chunk(runoff_data)
  #
  d_names <- names(runoff_data)
  d_nums <- d_names  %>% gsub('\\D','',.) %>% unique
  d_nums <- d_nums[!(d_nums == "")] %>% as.integer
  return(d_nums)
}



set_panel_size <- function(p=NULL, g=ggplotGrob(p), file=NULL, 
                           margin = unit(1,"mm"),
                           width=unit(3, "cm"), height=unit(2, "cm")){
  
  panels <- g$layout$name=="panel"
  panel_index_w<- g$layout$l[panels]
  panel_index_h<- g$layout$t[panels]
  nw <- length(unique(panel_index_w))
  nh <- length(unique(panel_index_h))
  g$widths[panel_index_w] <- rep(list(width), nw)
  g$heights[panel_index_h] <- rep(list(height), nh)
  class(g) <- c("fixed", class(g), "ggplot")
  if(!is.null(file)) 
    ggsave(file, g, 
           width = convertWidth(sum(g$widths) + margin, 
                                unitTo = "in", valueOnly = TRUE),
           height = convertHeight(sum(g$heights) + margin, 
                                  unitTo = "in", valueOnly = TRUE))
  g
}
