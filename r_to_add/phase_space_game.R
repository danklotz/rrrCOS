require(magrittr)
require(ggplot2)

runoff_example <- visCOS::get_runoff_example() %>% visCOS::prepare_complete_date()
data_to_shift <- runoff_example$QSIM_0002- runoff_example$QOBS_0002 
shft_dtfrm <- data.frame(time = 1:length(data_to_shift),
                         data = data_to_shift, 
                         data_shft = data_to_shift %>%
                            diff(.,1) %>% 
                            c(data_to_shift[1],.), 
                         yyyy = runoff_example$yyyy,
                         mm = runoff_example$mm
                         )

ggplot(shft_dtfrm) + 
  geom_path(aes(x = data, y = data_shft, col = yyyy)) + 
  facet_wrap(~mm)

ggplot(runoff_example) + 
  geom_point(aes(x = QOBS_0002, y = QOBS_0002-QSIM_0002, col = yyyy)) + 
  facet_wrap(~mm)

ggplot(runoff_example) + 
  geom_line(aes(x = posixdate, y = QOBS_0002), colour = "steelblue") + 
  geom_line(aes(x = posixdate, y = QSIM_0002), colour = "orange") + 
  theme_bw()
