  #' Get runoff example
  #'
  #' Get exemplary runoff data to test the different functions of visCOS
  #' @export
  
  get_runoff_example <- function() {
    file_path <- system.file("extdata",
                             "runoff_example.csv",
                             package = "visCOS")
    runoff_example <- read.csv(file_path)
    return(runoff_example)
  }
  #' removes chunk in runoff_data
  #'
  #' Removes all columns which are not foreseen (see: viscos_options) from
  #' runoff data
  #'
  #' @param runoff_data data.frame object containing at least COSdate,
  #' and two data rows defined in viscos_options
  #' @return data.frame object without the chunk
  #' 
  #' @import magrittr
  #' 
  #' @export

  remove_junk <- function(runoff_data) {
  assert_dataframe(runoff_data)
  # 
  lowercase_names_in_data <- runoff_data %>% names %>% tolower
  regex_columns <- get_regex_for_runoff_data() %>% tolower # see: helpers
  # 
  idx <- regex_columns %>%
    grep(.,lowercase_names_in_data)
  no_chunk_runoff_data <- runoff_data[ , idx]
  return( only_observed_basins(no_chunk_runoff_data) )
  }
  # remove basins without observations
  #
  # Removes basins without observation (-999/NA values) from the provided data.frame
  #
  # @param runoff_data A raw runoff_data data.frame, which may contains basins
  # without observations.
  # \strong{Note:} It is assumed that all available basins are simulated!
  # @return data.frame without the observation-free basins
  # 
  # @import magrittr
only_observed_basins <- function(runoff_data) {
  require("magrittr")
  assert_dataframe(runoff_data)
  chosen_cols <- which( names(runoff_data) != viscos_options("name_COSposix") )
  chosen_rows <- is.na(runoff_data[,chosen_cols])
  data_no_posix <- runoff_data[ ,chosen_cols]
  data_no_posix[chosen_rows] <- -999
  colmax <- lapply(X = data_no_posix, FUN = max) # get max of any column
  if ( any(colmax < 0.0) ){
    idx_temp <- which(colmax < 0.0)
    obs_regex <- paste(viscos_options("name_o"),".*", sep ="")
    OnlyQobsSelected <- idx_temp %>%
      names %>%
      tolower %>%
      grepl(obs_regex,.) %>%
      any
    if (!OnlyQobsSelected){
      stop("There are Qsim withouth simulation (i.e. only values smaller 0). Pls remove them first")
    }
    idx_slct <- c(idx_temp,idx_temp + 1) %>% sort()
    d_onlyObserved <- runoff_data[-idx_slct]
    # set remaining negative Qobs to NA, so that HydroGOF can be used correctly, also ignoring NAs
    colmin <- lapply(X = d_onlyObserved, FUN = min)
    idx_temp <- which(colmin < 0.0)
    d_onlyObserved[d_onlyObserved[idx_temp]<0, idx_temp] <- NA
    return(d_onlyObserved)
  } else {
    return(runoff_data)
  }
}


  #' Complete the date-formats with POSIXct or COSdate
  #'
  #' Complete the data-formats of your data.frame `POSIXct` and/or `COSdate`
  #'
  #' @param runoff_data The data.frame, which contains the runoff information
  #' @param name_cosyear string with the name of the `COSdate` year column
  #' @param name_posix string with the name of the POSIXct column
  #' @return The new runoff data.frame with the added data-format.
  #' 
  #' @import magrittr
  #' 
  #' @export
prepare_complete_date <- function(runoff_data = NULL,
                                  name_cosyear = "yyyy",
                                  name_posix = "POSIXdate") {
  # make sure that magrittr is loaded:
  assert_dataframe(runoff_data)
  # check for COSdates and stop if non-logical expression are obtained
  OK_COSdate <- any(names(runoff_data)== viscos_options("name_COSyear"))
  OK_POSIXdates <- any(names(runoff_data)== viscos_options("name_COSposix"))
  if ( !is.logical(OK_COSdate) | !is.logical(OK_POSIXdates) ) {
    stop("Something seems to be wrong with the date / time formats :(")
  }
  # choose function depending on which formats are available!
  if (!OK_COSdate & !OK_POSIXdates) {
    stop("No COSdates and no POSIXct-dates in the data!")
  } else if (OK_COSdate & !OK_POSIXdates) {
    runoff_data <- implode_cosdate(runoff_data) # see following chapter
  } else if (!OK_COSdate & OK_POSIXdates) {
    stop("POSIXct to COSdates not yet supported :(")
  }
  return(runoff_data)
}
# transform COSdate into the nicer POSIXct-date format
#
# Takes a data.frame, which contains the COSdate format (see: xxx) and
# transforms it into a POSIXct series. Note that time is assumed to be in UTC
implode_cosdate <- function(runoff_data) {
  require("magrittr", quietly = TRUE)
  assert_dataframe(runoff_data)
  name_string <-  runoff_data %>% names %>% tolower
  #
  POSIXdate <- paste(runoff_data[[viscos_options("name_COSyear")]],
                     sprintf("%02d",runoff_data[[viscos_options("name_COSmonth")]]),
                     sprintf("%02d",runoff_data[[viscos_options("name_COSday")]]),
                     sprintf("%02d",runoff_data[[viscos_options("name_COShour")]]),
                     sprintf("%02d",runoff_data[[viscos_options("name_COSmin")]]),
                     sep= "" ) %>%
    as.POSIXct(format = "%Y%m%d%H%M", origin = .[1], scale = "hourly", tz = "UTC")
  runoff_data[[viscos_options("name_COSposix")]] <- POSIXdate
  return(runoff_data)
}
# remove leading zeros from the names of runoff_data (data.frame)
remove_leading_zeros <- function(runoff_data) {
  require("magrittr", quietly = TRUE)
  runoff_data %<>% remove_junk
  runoff_names <- runoff_data %>% names
  runoff_lowercase_names <- runoff_names %>% tolower 
  #
  separator <- runoff_lowercase_names %>% 
    extract( grep(viscos_options()$name_o,.) ) %>% 
    extract( 1 ) %>%
    gsub(viscos_options()$name_o,"",.) %>% 
    gsub("\\d","",.)
  searchterm <- paste0(viscos_options()$name_o,"|", viscos_options()$name_s)
  runoff_nums <- runoff_lowercase_names %>% 
    gsub(searchterm,"",.) %>% 
    gsub(separator,"",.) %>% 
    gsub("\\D","",.)
  searchterm <- paste(runoff_nums, collapse = "")
  runoff_only_names <- runoff_names %>% 
    gsub(paste0("[",searchterm,"]"),"",.) %>% 
    gsub(separator,"",.)
  runoff_new_numbers <- runoff_nums %>% as.numeric() %>% as.character()
  runoff_new_numbers[is.na(runoff_new_numbers)] <- ""
  #
  names(runoff_data) <- runoff_new_numbers %>% 
    gsub("\\d+",separator,.) %>% 
    paste0(runoff_only_names,.,runoff_new_numbers)
  return(runoff_data)
}
  #' calculate periods
  #'
  #' Mark the periods within runoff_data.
  # The marking uses a monthly resolution, which are defined by the integers
  #' `start_month` and `end_month`.  
  #'
  #' @param runoff_data The data.frame, which contains the runoff information
  #' @return The runoff data.frame reduced and ordered according to the
  #' hydrological years within the data.
  #' \strong{Note:} The periods columns are formatted as characters!
  #' 
  #' @import dplyr
  #' @import magrittr
  #'
  #' @export
mark_periods <- function(runoff_data, start_month = 10, end_month = 9) {
  assert_dataframe(runoff_data)
  runoff_data %<>% remove_junk %>% prepare_complete_date()

  # (I) get labels for the months
  if (start_month <= end_month ) {
    period_range <- seq(start_month,end_month)
    out_of_period <- seq(1,12) %>% extract( !(seq(1,12) %in% period_range) )
  } else if (start_month > end_month) {
    range_1 <- seq(start_month,12)
    range_2 <- seq(1,end_month)
    period_range <- c(range_1,range_2)
    out_of_period <- seq(1,12) %>% extract( !(seq(1,12) %in% period_range) )
  }
  # (II) mark periods:
    eval_diff <- function(a) {c(a[1],diff(a))}
    runoff_data[[viscos_options("name_COSperiod")]] <- 
      runoff_data[[viscos_options("name_COSmonth")]] %in% c(start_month) %>%
      eval_diff %>%
      pmax(.,0) %>%
      cumsum
    runoff_data$period[runoff_data[[viscos_options("name_COSmonth")]] %in% out_of_period] <- 0
    # corrections for last year
    max_year <- max(runoff_data[[viscos_options("name_COSyear")]])
    runoff_data %<>% dplyr::mutate(
      period = ifelse(
        (  (.[[viscos_options("name_COSyear")]] == max_year) &
           (.[[viscos_options("name_COSmonth")]] > end_month)  ),
                      0,
                      period
        )
      )
    return(runoff_data)
}
  #' Convert runoff_data to xts-format
  #'
  #' Converts the runoff_data (class: data_frame) into an xts object
  #'
  #' @param runoff_data data_frame of the runoff_data (see: xxx)
  #' @return xts object of the runoff_data data.frame
  #' 
  #' @import zoo 
  #' @importFrom xts xts
  #' @import magrittr
  #' 
  #' @export
runoff_as_xts <- function(runoff_data) {
  # pre
  assert_dataframe(runoff_data)
  assert_chunk(runoff_data)
  assert_complete_date(runoff_data)
  # everything is set tolower because we try to keep visCOS case insensitive
  runoff_data <- remove_leading_zeros(runoff_data)

  names(runoff_data) <- names(runoff_data) %>% tolower
  name_posix <- viscos_options("name_COSposix") %>% tolower
  runoff_data_as_xts <- xts(x = runoff_data[], # ,names(runoff_data) != name_posix
                            order.by = runoff_data[[name_posix]])
  #
  return(runoff_data_as_xts)
}
