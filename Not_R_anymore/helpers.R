?ge# use max as boundary on data ---------------------------------------------
cut.lowerbound <- function(data,boundary) {data[data < boundary] <- boundary; return(data)}

# set panel size ----------------------------------------------------------
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



