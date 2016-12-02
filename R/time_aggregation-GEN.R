  #' Time Aggregation
  #' 
  #' Aggregates the cosero data.frame (\code{cos_data}) according to the 
  #' timely resolution defined via \code{aggregation}. Possible 
  #' resolution-choices are 'yyyy' - year, 'mm' - month and 'dd' - day and 
  #' combinations thereof. 
  #' 
  #' @param cos_data the COSERO data.frame as used within visCOS
  #' @param aggregation string that defines the resolution of the aggregation.
  #' @import magrittr
  #' @import ggplot2
  #' @import pasta
  #' @export
  aggregate_time <- function(cos_data, aggregation = "mm") {
    #
    cutting_bounds <- c(Inf,-Inf)
    if (grepl("dd",aggregation)) {
      cutting_bounds[1] <- min(9,cutting_bounds[1])
      cutting_bounds[2] <- max(11,cutting_bounds[2])
    }
    if (grepl("mm",aggregation)) {
      cutting_bounds[1] <- min(6,cutting_bounds[1])
      cutting_bounds[2] <- max(7,cutting_bounds[2])
    }
    if (grepl("yyyy",aggregation)) {
      cutting_bounds[1] <- min(1,cutting_bounds[1])
      cutting_bounds[2] <- max(4,cutting_bounds[2])
    }
    ######  define helpers
    regex_for_cos_selection <- viscos_options("name_data1") %|%  viscos_options("name_data2")
    # aggregation function:
    aggregator_fun <- function(k,data_frame){
      the_aggregation <- aggregate(data_frame[[k]] ~ data_frame$date_selection, FUN = mean)
      return(the_aggregation[ ,2])
    }
    ##### 
    cos_data %>% 
      visCOS::prepare_complete_date() %>% 
      visCOS::remove_chunk() -> full_cos_data 
    cos_with_aggreggation <- cbind.data.frame(
      full_cos_data,
      date_selection = substr(full_cos_data$posixdate,
                              cutting_bounds[1],
                              cutting_bounds[2]) %>% as.factor()
      )
    names_cos_selection <- grep(
      regex_for_cos_selection,
      names(cos_with_aggreggation) %>% tolower, 
      value = TRUE
      )
    selected_cos_rows <- grep(regex_for_cos_selection,
                             names(cos_with_aggreggation), 
                             ignore.case = TRUE)
    selected_cos_rows %>% 
      sapply(.,function(x) aggregator_fun(x,cos_with_aggreggation)) %>% 
      data.frame(idx = 1:nrow(.), 
                 time_aggregate = unique(cos_with_aggreggation$date_selection),
                 .) %>% 
      set_names(., c("idx","time_aggregate",names_cos_selection)) -> time_aggregate
    # 
    melted_time_aggregate <- time_aggregate %>% 
      reshape2::melt(., id.vars = c("idx","time_aggregate")) %>% 
      cbind.data.frame(., 
                       basin =  .$variable %>%
                         gsub(regex_for_cos_selection,"",.) %>% 
                         gsub("\\D","",.) %>%
                         as.integer, 
                       obs_sim = .$variable %>% 
                         gsub(viscos_options("name_data1") %&% ".*",viscos_options("name_data1"),.) %>% 
                         gsub(viscos_options("name_data2") %&% ".*",viscos_options("name_data2"),.))
    return(melted_time_aggregate)
  }
