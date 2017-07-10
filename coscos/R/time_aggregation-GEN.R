  #' Time Aggregation
  #'
  #' Aggregates the COSERO data.frame (\code{cos_data}) according to the
  #' timely resolution defined via \code{aggregation}. Possible
  #' resolution-choices are \code{'yyyy'} - year, \code{'mm'} - month and
  #' \code{'dd'} - day and combinations thereof.
  #'
  #' @param cos_data the COSERO data.frame as used within visCOS
  #' @param aggregation string that defines the resolution of the aggregation.
  #' @import pasta
  #' 
  #' @export
  aggregate_time <- function(cosdata, key = "mm", .funs = mean, opts = coscos::viscos_options()) {
    le_data <- cook_cosdate(cosdata)
    le_names <- name(le_data)
    le_aggr <- clump(le_data, key = key, .funs = .funs) 
    le_aggr[opts$name_COSposix] <- le_data[[opts$name_COSposix]] %>% 
      clump_posix(.,key = key)
    # melt the data in a tidy format:
    selected_cols <- (opts$name_o %|% opts$name_s) %>% 
      grepl(.,le_names, ignore.case = TRUE) %>% 
      le_names[.]
    tidy_aggr <- tidyr::gather_(le_aggr,
                                key_col = c("key"),
                                value_col = "value", 
                                gather_cols = selected_cols)
    
    melted_time_aggregate <- le_aggr %>%
      reshape2::melt(., id.vars = c(key,"date")) %>%
      cbind.data.frame(.,
                       basin =  .$variable %>%
                         gsub(regex_for_cos_selection,"",.) %>%
                         gsub("\\D","",.) %>%
                         as.integer,
                       obs_sim = .$variable %>%
                         gsub(viscos_options("name_o") %&% ".*",viscos_options("name_o"),.) %>%
                         gsub(viscos_options("name_s") %&% ".*",viscos_options("name_s"),.))
    return(melted_time_aggregate)
  }
