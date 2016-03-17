#' Get some objective functions (OF)
#'
#' Get some basic objective functions used in hydrology, i.e.: Root Mean Squared Error, Correlation, NSE, KGE, pbias
#' @return data.frame contianing basic OF
#' @export
serve_ofun <- function(obs,sim) {
  require("hydroGOF", quietly = TRUE)
  require("magrittr", quietly = TRUE)
  # calc
  out <- data.frame(
    RMSE = -999,
    pbias = -999,
    NSE = -999,
    KGE = -999,
    corr = -999,
    beta =  -999,
    alpha =  -999
  )
  out$RMSE <- rmse(sim,obs) %>% as.numeric
  out$NSE <- NSE(sim,obs) %>% as.numeric
  out$pbias <- pbias(sim,obs)
  out$KGE <- KGE(sim,obs) %>% as.numeric
  out$corr <- cor(obs,sim) 
  out$beta <- tmp[1] %>% as.numeric
  out$alpha <- tmp["KGE.elements.r"] %>% as.numeric
  return(out)
}
