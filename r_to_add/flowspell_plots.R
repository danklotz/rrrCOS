# Testing hte use of flow spell plots to give a general overview of a basin
require(visCOS)
require(magrittr)
require(dplyr)
require(ggplot2)
# 
tr_a <- 0.1
tr_b <- 0.4

runoff_data <- get_runoff_example() %>% 
  remove_chunk() %>% 
  prepare_complete_date() %>% 
  mark_periods()
# get sub selection strings
selection_string <- paste(viscos_options("name_data1"),".*|",
                          viscos_options("name_data2"),".*", 
                          sep = "")
data_selection <- grepl(selection_string,names(runoff_data), ignore.case = TRUE)
# define functions for data normalisation:
range01 <- function(x){(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
normalise_data <- function(data_,data_selection) {
  normalised_data <- apply(data_[,data_selection],2,range01)
  data_[data_selection] <- normalised_data
  return(data_)
}
# melt data 
melted_runoff_data <- runoff_data %>% 
  dplyr::filter(period > 0) %>% 
  normalise_data(.,data_selection) %>% 
  reshape2::melt(id.vars = c("period","yyyy","mm","dd","hh","posixdate","min")) 
# 
marked_data <- melted_runoff_data %>% 
  dplyr::mutate(cut_marks = ifelse(value < tr_a,
                                   "below_tr_a",
                                   ifelse(value < tr_b,
                                          "above_tr_a",
                                          "above_tr_b"))) %>% 
  dplyr::mutate(basin = gsub(viscos_options("name_data1"),"",variable) %>% 
                  gsub( viscos_options("name_data2"),"",.) %>% 
                  gsub("\\D","",.) %>% 
                  as.integer())
# marked_data$group_obs <- c( 0,diff(marked_data$cut_marks_obs) ) %>%
#   equals(0) %>% 
#   not %>% 
#   cumsum() %>% 
#   add(1)
viscos_options(color_of_low = "purple4", 
               color_of_mid = "steelblue", 
               color_of_high = "cyan")
ggplot(marked_data) + 
  geom_raster(aes(x = posixdate, y = variable, fill = cut_marks)) +
  scale_fill_manual(values = c("below_tr_a" = viscos_options("color_of_low"),
                               "above_tr_a" = viscos_options("color_of_mid"),
                               "above_tr_b"  = viscos_options("color_of_high")), 
                    breaks = c("below_tr_a","above_tr_a","above_tr_b"), 
                    labels = c(paste("below ",tr_a,sep = ""),
                              paste("above ",tr_a,sep = ""),
                              paste("above ",tr_b,sep = ""))) + 
  facet_wrap(~basin, scales = "free_y",ncol = 1)
# note that this is probably more informative for modellers than: 
ggplot(marked_data) +
  geom_line(aes(x = posixdate, y= value, color = variable)) + 
  geom_hline(aes(yintercept=tr_a)) + 
  geom_hline(aes(yintercept=tr_b)) + 
  facet_wrap(~basin, ncol = 1) 
