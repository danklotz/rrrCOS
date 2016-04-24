test_KGE <- function(obs,sim) {
  # def:
    require(hydroGOF,quietly = TRUE)
    
  # calc:
  length_loop <- length(obs)
  ref_kge <- hydroGOF::KGE(sim,obs)
  change_KGE <- seq(1,length_loop)
  test <- sapply(change_KGE, sub_fun, obs = obs, sim = sim, ref_kge =ref_kge) 
  
} 
 
 
  sub_fun <- function(x,obs,sim,ref_kge) {
    require(hydroGOF,quietly = TRUE)
    x_obs <- c(obs[1:x-1],obs[x+1:length_loop])
    x_sim <- c(sim[1:x-1],sim[x+1:length_loop])
    x_kge <- hydroGOF::KGE(x_sim,x_obs)
    return(x_kge)
  }
  
  
  