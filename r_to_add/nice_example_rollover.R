require(visCOS)
require(dygraphs)
require(xts)
require(dplyr)

runoff_example <- get_runoff_example() %>% remove_chunk()
runoff_example_norm <- dplyr::select(runoff_example,starts_with("q")) %>%  
  scale(., center=FALSE, scale=colSums(.)) %>%
  cbind.data.frame(select(runoff_example,matches("yyyy|mm|dd|hh|min")),.) %>% 
  visCOS::prepare_complete_date()
runoff_example_norm_xts <- dplyr::select(runoff_example_norm,matches("QOBS_0001|QSIM_0001")) %>% 
  xts(., order.by = runoff_example_norm$posixdate)
dygraph(runoff_example_norm_xts) %>% 
  dySeries("QOBS_0001",
           label = visCOS::viscos_options("name_data1"),
           color = viscos_options("color_data1")) %>%
  dySeries("QSIM_0001",
           label = visCOS::viscos_options("name_data2"),
           color = viscos_options("color_data2")) %>%
  dyRoller(rollPeriod = 5) 
