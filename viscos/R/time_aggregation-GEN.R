  #' Time Aggregation
  #'
  #' Aggregates the COSERO data.frame (\code{cosdata}) according to the
  #' timely resolution defined via \code{aggregation}. Possible
  #' resolution-choices are \code{'yyyy'} - year, \code{'mm'} - month and
  #' \code{'dd'} - day and combinations thereof.
  #'
  #' @param cosdata The strictly defined data format (\code{cosdata}) used 
  #'          within \pkg{viscos}
  #' @param aggregation A string that defines the resolution of the aggregation.
  #' 
  #' @import coscos
  #' @import pasta
  #' @importFrom tidyr gather_
  #' @export
  aggregate_time <- function(cosdata, 
                             key = "mm", 
                             .funs = base::mean, 
                             opts = coscos::viscos_options()) {
    if (class(key) != "character")
      stop("key must be a chracter. It currently is:" %&&% class(key))
    le_data <- coscos::cook_cosdata(cosdata)
    le_aggr <- coscos::clump(le_data, key = key, .funs = .funs) 
    le_aggr[opts$name_COSposix] <- le_data[[opts$name_COSposix]] %>% 
      coscos::clump_posix(.,key = key)
    le_names <- names(le_aggr)
    # melt the data in a tidy format:
    selected_cols <- (opts$name_o %|% opts$name_s) %>% 
      grepl(.,le_names, ignore.case = TRUE) %>% 
      le_names[.]
    tidy_aggr <- tidyr::gather_(le_aggr,
                                key_col = c("key"),
                                value_col = "value", 
                                gather_cols = selected_cols)
    return(tidy_aggr)
  }

  
  # functions to extrapolte additional information:
    # remove_basin_nums <- function(basin_names) {
    #   basin_names %>% 
    #     gsub(opts$name_o %&% ".*", opts$name_o , ., ignore.case = TRUE) %>% 
    #     gsub(opts$name_s %&% ".*", opts$name_s, ., ignore.case = TRUE)
    # }
    # remove_basin_names <- function(basin_names) {
    #   as.integer(gsub("\\D","",basin_names))
    # }
