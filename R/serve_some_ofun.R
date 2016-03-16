#' Get some objective functions (OF)
#'
#' Get some basic objective functions used in hydrology, i.e.: Root Mean Squared Error, Correlation, NSE, KGE, pbias
#' @return data.frame contianing basic OF
#' @export
serve_ofun <- function(obs,sim) {
  require(hydroGOF)
  require(magrittr)
  # calc
  out <- data.frame(
    RMSE = -999,
    corr = -999,
    NSE = -999,
    KGE = -999,
    pbias = -999
  )
  out$RMSE <- rmse(sim,obs) %>% as.numeric
  out$corr <- cor(sim,obs) %>% diag(.)
  out$NSE <- NSE(sim,obs) %>% as.numeric
  out$KGE <- KGE(sim,obs) %>% as.numeric
  out$pbias <- pbias(sim,obs)
  return(out)
}
