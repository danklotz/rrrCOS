#' Get basic objective function for runoff_data
#'
#' Calculate basic objective functions(NSE, KGE, percentage BIAS, Correlation) 
#' for every basin and the chosen periods.
#'
#' @param runoff_data runoff_data data.frame.
#' @return list of basic objective function evaluated for the different
#' hydrological years and over the whole timespan.
#' 
#' @import hydroGOF 
#' @import dplyr
#'
#' @export
extract_objective_functions <- function(runoff_data) {
  assert_dataframe(runoff_data)
  if( !exists(viscos_options("name_COSperiod"), where = runoff_data) ) {
    stop("Error! Period-Column missing in runoff_data; use `mark_periods`")
  }
  # (I) reduce necessary computation
  evaluation_data <- 
    runoff_data[ runoff_data[[viscos_options("name_COSperiod")]] > 0, ]
  # (II) get information
  number_of_basins <- evaluation_data %>%
    names %>%
    unique %>%
    tolower %>%
    grepl(viscos_options("name_data1") , .) %>%
    sum
  periods_in_data <- evaluation_data[[viscos_options("name_COSperiod")]] %>%
    unique
  number_of_periods <- periods_in_data %>% length

  # (III) calculate overall objective functions
  temp_x <- dplyr::select(evaluation_data,starts_with(viscos_options("name_data1"))) %>%
    unname
  temp_y <- dplyr::select(evaluation_data,starts_with(viscos_options("name_data2"))) %>%
    unname
  nse_ <- hydroGOF::NSE(temp_y,temp_x)
  kge_ <- hydroGOF::KGE(temp_y,temp_x)
  pbias_ <- hydroGOF::pbias(temp_y,temp_x)
  corr_ <- cor(temp_y,temp_x) %>% diag(.)

  # (IV) calulcated period-vise objective functions
    # pre allocation of periodic variables:
    NSE_period <- matrix(nrow = number_of_periods, ncol = as.integer(number_of_basins), data = NA)
    KGE_period <- NSE_period
    pBIAS_period <- NSE_period
    CORR_period <- NSE_period

    # calculation loop # proabbly slow
    for (k in 1:number_of_periods) {
      temp_x <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options("name_data1"))) %>%
        unname
      temp_y <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options("name_data2"))) %>%
        unname
      NSE_period[k,1:number_of_basins] <- hydroGOF::NSE(temp_y,temp_x)
      KGE_period[k,1:number_of_basins] <- hydroGOF::KGE(temp_y,temp_x)
      pBIAS_period[k,1:number_of_basins] <- hydroGOF::pbias(temp_y,temp_x)
      CORR_period[k,1:number_of_basins] <- cor(temp_y,temp_x) %>% diag(.)
    }
  #
  obj_names <- c("NSE","KGE","pBIAS","CORR", 
                    paste("NSE_period",1:number_of_periods,sep="."), 
                    paste("KGE_period",1:number_of_periods,sep="."),
                    paste("pBIAS_period",1:number_of_periods,sep="."),
                    paste("CORR_period",1:number_of_periods,sep=".")
  )
  obj_fun <- data.frame(of = obj_names, 
                        basin = rbind(nse_,
                                      kge_,
                                      pbias_,
                                      corr_,
                                      NSE_period,
                                      KGE_period,
                                      pBIAS_period,
                                      CORR_period),
                        row.names = NULL)
  return(obj_fun)
}

