#' Get basic objective function for runoff_data
#'
#' Calculate basic objective functions (i.e. NSE, KGE, percentage BIAS, COrrelation (see: xxx)) for
#' every basin and hydrological year
#'
#' @param runoff_data runoff_data data.frame (see:xxx).
#' @return list of baisc objective function evaluated for the different hydrological years and over the whole timespan.
#' @export
pour_period_ofun <- function(runoff_data) {
  require(hydroGOF, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  assert_dataframe(runoff_data)
  stopifnot( exists("period", where = runoff_data) )
  #
  periods_in_data <- which(unique(runoff_data$period) > 0)
  num_periods <- length(periods_in_data)
  runoff_data %<>% filter(period == periods_in_data)
  eval_size <- runoff_data %>% names %>% unique %>% tolower %>% grepl("qobs.*",.) %>% sum
  Ofun  <- list()
  Ofun$NSE_periods <- matrix(nrow = num_periods, ncol = as.integer(eval_size), data = NA)
  Ofun$KGE_periods <- Ofun$NSE_periods
  Ofun$pBIAS_periods <- Ofun$NSE_periods
  Ofun$CORR_periods <- Ofun$NSE_periods
  for (k in 1:num_periods)
  {
    tempOBS <- filter(runoff_data,period == periods_in_data[k]) %>% select(.,starts_with("qobs"))
    tempSIM <- filter(runoff_data,period == periods_in_data[k]) %>% select(.,starts_with("qsim"))
    Ofun$NSE_periods[k,1:eval_size] <- hydroGOF::NSE(tempSIM,tempOBS)
    Ofun$KGE_periods[k,1:eval_size] <- hydroGOF::KGE(tempSIM,tempOBS)
    Ofun$pBIAS_periods[k,1:eval_size] <- hydroGOF::pbias(tempSIM,tempOBS)
    Ofun$CORR_periods[k,1:eval_size] <- cor(tempSIM,tempOBS) %>% diag(.)
  }
  tempOBS <- select(runoff_data,starts_with("qobs"))
  tempSIM <- select(runoff_data,starts_with("qsim"))
  Ofun$NSE <- hydroGOF::NSE(tempSIM,tempOBS)
  Ofun$KGE <- hydroGOF::KGE(tempSIM,tempOBS)
  Ofun$pBIAS <- hydroGOF::pbias(tempSIM,tempOBS)
  Ofun$CORR <- cor(tempSIM,tempOBS) %>% diag(.)
  #
  return(Ofun)
}
