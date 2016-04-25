test_KGE <- function(obs,sim) {
  # def:
    require(hydroGOF,quietly = TRUE)
    require(dplyr, quietly = TRUE)
  # calc:
  length_loop <- length(obs)
  data <- data.frame(idx = 1:length_loop,obs = obs, sim = sim)
  sub_fun <- function(data, x) {
    require(hydroGOF,quietly = TRUE)
    require(dplyr, quietly = TRUE)
    #
    mod_data <- filter(data, idx != x)
    x_kge <- hydroGOF::KGE(mod_data$sim,mod_data$obs)
    return(x_kge)
  }
  change_KGE <- seq(1,length_loop)
  test <- sapply(change_KGE, sub_fun, data = data) 
  
} 
#
require("data.table")
require("magrittr", quietly = TRUE)
pathDotRunoff  <- file.choose()
d_raw <- fread(pathDotRunoff, check.names = TRUE, header = TRUE, skip = 22) %>%
  as.data.frame(.)
names(d_raw)[5] <- "min"
#
obs <- d_raw$QOBS_0003
sim <- d_raw$QSIM_0003
abs_deviation <- test_KGE(obs,sim)
rel_deviation <- abs_deviation/hydroGOF::NSE(sim,obs)
# plot:
plot(obs,type = "l", col = "blue"); 
par(new = T); 
plot(sim, type = "l", col = "red",  axes= FALSE, xlab= NA, ylab=NA);
par(new = T)
plot(rel_deviation, type = "l", col = "magenta",  axes= FALSE, xlab=NA, ylab=NA); 
axis(side = 4);
mtext(side = 4, line = 3, 'relative change of KGE')
  


# test_KGE <- function(obs,sim) {
#   # def:
#   require(hydroGOF,quietly = TRUE)
#   
#   # calc:
#   length_loop <- length(obs)
#   sub_fun <- function(x,obs,sim,ref_kge) {
#     require(hydroGOF,quietly = TRUE)
#     x_obs <- c(obs[1:(x-1)],obs[(x+1):length_loop])
#     x_sim <- c(sim[1:(x-1)],sim[(x+1):length_loop])
#     x_kge <- hydroGOF::KGE(x_sim,x_obs)
#     return(x_kge)
#   }
#   ref_kge <- hydroGOF::KGE(sim,obs)
#   change_KGE <- seq(1,length_loop)
#   test <- sapply(change_KGE, sub_fun, obs = obs, sim = sim, ref_kge =ref_kge) 
#   
# } 
  
  