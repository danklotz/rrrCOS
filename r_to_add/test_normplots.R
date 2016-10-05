require(dplyr)
require(ggplot2)
require(visCOS)

runoff_data <- visCOS::get_runoff_example() %>% remove_chunk()
norm_runoff_data <- runoff_data %>% 
  dplyr::select(.,starts_with("q")) %>%  
  scale(., center=FALSE, scale=colSums(.)) %>%
  cbind.data.frame(select(runoff_data,matches("yyyy|mm|dd|hh|min")),.) %>% 
  visCOS::prepare_complete_date()

ggplot(norm_runoff_data) + 
  geom_line(aes(x=posixdate,y=QOBS_0001), color = "red", alpha = 0.2)+ 
  geom_line(aes(x=posixdate,y=QOBS_0002), color = "red", alpha = 0.2) 
