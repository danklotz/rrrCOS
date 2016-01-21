#' Get basic objective function for runoff_data
#'
#' Calculate basic objective functions (i.e. NSE, KGE, percentage BIAS, COrrelation (see: xxx)) for
#' every basin and hydrological year
#'
#' @param runoff_data runoff_data data.frame (see:xxx).
#' @return list of baisc objective function evaluated for the different hydrological years and over the whole timespan.
#' @export
fetch.some_ofun_4_hydyears <- function(runoff_data, hydyears_in_data) {
  require(hydroGOF)
  require(dplyr)
  assert.dataframe(runoff_data)
  assert.dataframe(runoff_data)
  #
  eval_size <- runoff_data %>% names %>% unique %>% tolower %>% grepl("qobs.*",.) %>% sum
  num_hydyears <- length(hydyears_in_data)
  Ofun  <- list()
  Ofun$NSE.hydyearly <- matrix(nrow = num_hydyears, ncol = as.integer(eval_size), data = NA)
  Ofun$KGE.hydyearly <- Ofun$NSE.hydyearly
  Ofun$pBIAS.hydyearly <- Ofun$NSE.hydyearly
  Ofun$CORR.hydyearly <- Ofun$NSE.hydyearly
  for (k in 1:num_hydyears)
  {
    tempOBS <- filter(runoff_data,hydyear == hydyears_in_data[k]) %>% select(.,starts_with("qobs"))
    tempSIM <- filter(runoff_data,hydyear == hydyears_in_data[k]) %>% select(.,starts_with("qsim"))
    Ofun$NSE.hydyearly[k,1:eval_size] <- hydroGOF::NSE(tempSIM,tempOBS)
    Ofun$KGE.hydyearly[k,1:eval_size] <- hydroGOF::KGE(tempSIM,tempOBS)
    Ofun$pBIAS.hydyearly[k,1:eval_size] <- hydroGOF::pbias(tempSIM,tempOBS)
    Ofun$CORR.hydyearly[k,1:eval_size] <- cor(tempSIM,tempOBS) %>% diag(.)
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
