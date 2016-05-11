data_Mur_raw <- read.table("C:/Users/H0740147/Cosero_Mur/COSERO_Mur_Final/COSERO_Anwendung/output_60min_final2411/qobs_qsim.txt", header = TRUE)
d_runoff <- data_Mur_raw %>% prepare.remove_chunk %>% prepare.only_observed
d_runoff <- prepare(this = complete_date, from_that = d_runoff)
d_runoff <- prepare.periods(d_runoff, start_month = 9, end_month = 8)
bOF <- serve.period_ofun(d_runoff)



d_runoff <- prepare.remove_chunk(data_Mur_raw)
d_only_observed <- prepare.only_observed(d_runoff)
head(d_only_observed)
plot(data_Mur$Qobs_0001,
     type = "l",
     col = "steelblue",
     ylab = "runoff",
     xlab = "time")
lines(data_Mur$Qsim_0001,
      col = "palevioletred")