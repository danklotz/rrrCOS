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
  #' removes junk in cos_data
  #'
  #' Removes all columns which are not foreseen (see: viscos_options) from
  #' runoff data
  #'
  #' @param cos_data The cos_data data.frame (see vignette for info)
  #' @return data.frame object without the chunk
  #'
  #' @import magrittr
  #'
  #' @export

  remove_junk <- function(cos_data) {
  assert_dataframe(cos_data)
  #
  names_in_data <- cos_data %>% names
  regex_columns <- get_regex_for_cos_data() # see: helpers
  #
  idx <- regex_columns %>%
    grep(.,names_in_data, ignore.case = TRUE)
  no_junk_cos_data <- cos_data[ , idx]
  return( only_observed_basins(no_junk_cos_data) )
  }
  # remove basins without observations
  #
  # Removes basins without observation (-999/NA values) from the provided data.frame
  #
  # @param cos_data A raw cos_data data.frame, which may contains basins
  # without observations.
  # \strong{Note:} It is assumed that all available basins are simulated!
  # @return data.frame without the observation-free basins
  #
  # @import magrittr
  only_observed_basins <- function(cos_data) {
    require("magrittr")
    assert_dataframe(cos_data)
    chosen_cols <- which( names(cos_data) != viscos_options("name_COSposix") )
    chosen_rows <- is.na(cos_data[,chosen_cols])
    data_no_posix <- cos_data[ ,chosen_cols]
    data_no_posix[chosen_rows] <- viscos_options("missing_data")
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
      d_onlyObserved <- cos_data[-idx_slct]
      # set remaining negative Qobs to NA, so that HydroGOF can be used correctly, also ignoring NAs
      colmin <- lapply(X = d_onlyObserved, FUN = min)
      idx_temp <- which(colmin < 0.0)
      d_onlyObserved[d_onlyObserved[idx_temp]<0, idx_temp] <- NA
      return(d_onlyObserved)
    } else {
      return(cos_data)
    }
  }
  #' Complete the date-formats with POSIXct or COSdate
  #'
  #' Complete the data-formats of your data.frame `POSIXct` and/or `COSdate`
  #'
  #' @param cos_data The data.frame, which contains the runoff information
  #' @param name_cosyear string with the name of the `COSdate` year column
  #' @param name_posix string with the name of the POSIXct column
  #' @return The new runoff data.frame with the added data-format.
  #'
  #' @import magrittr
  #'
  #' @export
complete_dates <- function(cos_data = NULL,
                                  name_cosyear = "yyyy",
                                  name_posix = "POSIXdate") {
  # make sure that magrittr is loaded:
  assert_dataframe(cos_data)
  # check for COSdates and stop if non-logical expression are obtained
  OK_COSdate <- any(names(cos_data)== viscos_options("name_COSyear"))
  OK_POSIXdates <- any(names(cos_data)== viscos_options("name_COSposix"))
  if ( !is.logical(OK_COSdate) | !is.logical(OK_POSIXdates) ) {
    stop("Something seems to be wrong with the date / time formats :(")
  }
  # choose function depending on which formats are available!
  if (!OK_COSdate & !OK_POSIXdates) {
    stop("No COSdates and no POSIXct-dates in the data!")
  } else if (OK_COSdate & !OK_POSIXdates) {
    cos_data <- implode_cosdate(cos_data) # see following chapter
  } else if (!OK_COSdate & OK_POSIXdates) {
    stop("POSIXct to COSdates not yet supported :(")
  }
  return(cos_data)
}
# transform COSdate into the nicer POSIXct-date format
#
# Takes a data.frame, which contains the COSdate format (see: xxx) and
# transforms it into a POSIXct series. Note that time is assumed to be in UTC
implode_cosdate <- function(cos_data) {
  require("magrittr", quietly = TRUE)
  assert_dataframe(cos_data)
  name_string <-  cos_data %>% names %>% tolower
  #
  POSIXdate <- paste(cos_data[[viscos_options("name_COSyear")]],
                     sprintf("%02d",cos_data[[viscos_options("name_COSmonth")]]),
                     sprintf("%02d",cos_data[[viscos_options("name_COSday")]]),
                     sprintf("%02d",cos_data[[viscos_options("name_COShour")]]),
                     sprintf("%02d",cos_data[[viscos_options("name_COSmin")]]),
                     sep= "" ) %>%
    as.POSIXct(format = "%Y%m%d%H%M", origin = .[1], scale = "hourly", tz = "UTC")
  cos_data[[viscos_options("name_COSposix")]] <- POSIXdate
  return(cos_data)
}
# remove leading zeros from the names of cos_data (data.frame)
remove_leading_zeros <- function(cos_data) {
  require("magrittr", quietly = TRUE)
  cos_data %<>% remove_junk
  runoff_names <- cos_data %>% names
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
  names(cos_data) <- runoff_new_numbers %>%
    gsub("\\d+",separator,.) %>%
    paste0(runoff_only_names,.,runoff_new_numbers)
  return(cos_data)
}
  #' calculate periods
  #'
  #' Mark the periods within cos_data.
  # The marking uses a monthly resolution, which are defined by the integers
  #' `start_month` and `end_month`.  
  #'
  #' @param cos_data The data.frame, which contains the runoff information
  #' @return The runoff data.frame reduced and ordered according to the
  #' hydrological years within the data.
  #' \strong{Note:} The periods columns are formatted as characters!
  #'
  #' @import dplyr
  #' @import magrittr
  #'
  #' @export
mark_periods <- function(cos_data, start_month = 10, end_month = 9) {
  assert_dataframe(cos_data)
  cos_data %<>% remove_junk %>% complete_dates()
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
    cos_data[[viscos_options("name_COSperiod")]] <-
      cos_data[[viscos_options("name_COSmonth")]] %in% c(start_month) %>%
      eval_diff %>%
      pmax(.,0) %>%
      cumsum
    cos_data$period[cos_data[[viscos_options("name_COSmonth")]] %in% out_of_period] <- 0
    # corrections for last year
    max_year <- max(cos_data[[viscos_options("name_COSyear")]])
    cos_data %<>% dplyr::mutate(
      period = ifelse(
        (  (.[[viscos_options("name_COSyear")]] == max_year) &
           (.[[viscos_options("name_COSmonth")]] > end_month)  ),
                      0,
                      period
        )
      )
    return(cos_data)
}
  #' Convert cos_data to xts-format
  #'
  #' Converts the cos_data (class: data_frame) into an xts object
  #'
  #' @param cos_data data_frame of the cos_data (see: xxx)
  #' @return xts object of the cos_data data.frame
  #'
  #' @import zoo
  #' @importFrom xts xts
  #' @import magrittr
  #'
  #' @export
runoff_as_xts <- function(cos_data) {
  # pre
  assert_dataframe(cos_data)
  assert_chunk(cos_data)
  assert_complete_date(cos_data)
  # everything is set tolower because we try to keep visCOS case insensitive
  cos_data <- remove_leading_zeros(cos_data)

  names(cos_data) <- names(cos_data) %>% tolower
  name_posix <- viscos_options("name_COSposix") %>% tolower
  cos_data_as_xts <- xts(x = cos_data[], # ,names(cos_data) != name_posix
                            order.by = cos_data[[name_posix]])
  #
  return(cos_data_as_xts)
}
