# ---------------------------------------------------------------------------
# Code for cooking data
# authors: Daniel Klotz, Johannes Wesemann, Mathew Herrnegger
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# -------------------------------------------------------------------------
# cosdata generator: 
setClass("cosdata",
         slots = c(start_date = "character",
                   end_date = "character",
                   meta_info = "character"),
         contains = "data.frame")
# -------------------------------------------------------------------------
#' Cook cos_data 
#' 
#' `cos_data` builder function
#' 
#' @import pasta
#' @export
cook_cosdata <- function(data_frame, info = "") {
  # if input is already cosdata, return it unchanged:
  if (class(data_frame) == "cosdata") {
    return(data_frame)
  }
  # transformation: =======================================================
  le_cos <- data_frame %>% 
    build_tibble(.) %>% 
    remove_junk(.) %>% 
    complete_dates(.) %>% 
    mark_periods(.)
  # checks: ===============================================================
  # check for names: 
  data_names <- names(cos_data)
  check_names <- grepl(get_regex_for_cos_data(),
                       data_names,
                       ignore.case = TRUE)
  if( any(check_names == FALSE) ) {
    error_message <- "Cannot remove all unwanted columns." %&&%
                     "Try to remove them manually:" %&&%
                     "\n        " %&&%
      paste(data_names[1 - check_names], collapse = ", ")
    stop(error_message)
  } 
  # check for dimensions:
  n_cols <- ncol(le_cos)
  n_data_cols <- n_cols-7 # 7 == amount of other columns in data 
  n_date_cols <- n_cols - n_data_cols - 2 # must be 5: yyyy,mm,dd,hh,min
  if ((n_data_cols %% 2) != 0) stop("the number of o- and s-columns must be equal!")
  if ((ncols - n_osdata - 2) != 5) stop("cos-date columns are not complete")
  # add meta-data: ========================================================
  new("cosdata",
      start_date = le_cos$posixdate[[1]] %>% as.character(.),
      end_date = le_cos$posixdate[[length(le_cos)]] %>% as.character(.),
      meta_info = info)
}
  # --------------------------------------------------------------------------
  #' Get runoff example
  #'
  #' Get exemplary runoff data to test the different functions of visCOS
  #' @export
  get_viscos_example <- function( ) {
    path <- system.file("extdata", "runoff_example.csv", package = "visCOS")
    runoff_example <- read.csv(path)
    return(runoff_example)
  }

  # --------------------------------------------------------------------------
  #' removes junk in cos_data
  #'
  #' Removes all columns which are not foreseen (see: viscos_options) from
  #' runoff data
  #'
  #' @import magrittr
  #' @param cos_data The cos_data data.frame (see vignette for info)
  #' @return data.frame object without the chunk
  #' @export
  remove_junk <- function(cos_data) {
    assert_dataframe(cos_data) # see: defensive code
    # determine names of cos_data and get regex:
    names_in_data <- cos_data %>% names(.)
    regex_columns <- get_regex_for_cos_data( ) # see: helpers
    # get idx and clean data: ================================================
    idx <- grep(regex_columns,names_in_data, ignore.case = TRUE)
    clean_cos_data <- only_observed_basins(cos_data[ ,idx])
    return( clean_cos_data )
  }

  # ---------------------------------------------------------------------------
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
  # @import pasta
  only_observed_basins <- function(cos_data) {
    # pre: ====================================================================
    require("magrittr")
    require("pasta")
    assert_dataframe(cos_data)
    missing_data_marker <- viscos_options("missing_data")
    # check for missing obs: ==================================================
    # set NA values to viscos_options("missing_data") and check if there are
    # cloumns wihtouth observervation:
    chosen_cols <- which( names(cos_data) != viscos_options("name_COSposix") )
    rows_with_na <- is.na(cos_data[ ,chosen_cols])
    data_wihtouth_posix <- cos_data[ ,chosen_cols]
    data_wihtouth_posix[rows_with_na] <- missing_data_marker
    colmax <- sapply(X = data_wihtouth_posix, FUN = max)
    # remove unobserved pairs: ================================================
    if ( any(colmax < 0.0) ){
      name_o <- viscos_options("name_o")
      neg_o_names <- which(colmax < 0.0) %>% names(.)
      neg_s_names <- gsub(name_o,viscos_options("name_s"),
                          neg_o_names,
                          ignore.case = TRUE)
      data_selection <-  paste(neg_o_names,
                               neg_s_names,
                               sep = "|",
                               collapse = "|") %>%
        grepl(names(cos_data), ignore.case = TRUE) %>%
        not(.)
      data_only_observed <- cos_data[ ,data_selection]
    } else {
      data_only_observed <- cos_data
    }
    # bonus: change missing_data to NA (useful for of computation) ============
    idx_NA <- data_only_observed %>% equals(missing_data_marker)
    data_only_observed[idx_NA] <- NA
    return(data_only_observed)
  }

  # ---------------------------------------------------------------------------
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
  complete_dates <- function(cos_data) {
    # pre: ====================================================================
    assert_dataframe(cos_data)
    date_names <- unlist(viscos_options("name_COSyear",
                                        "name_COSmonth",
                                        "name_COSmonth",
                                        "name_COShour",
                                        "name_COSmin")
                        )
    # check dates: ============================================================
    # stop if non-logical expression are obtained
    all_dates_in_cosdata <- any( date_names %in% names(cos_data) )
    posix_cosdata <- any(viscos_options("name_COSposix") == names(cos_data))
    if ( !is.logical(all_dates_in_cosdata) | !is.logical(posix_cosdata) ) {
      stop("Something is wrong :( \n
            Some of the date-columns could not be processed!")
    }
    # execute function for the available format: ==============================
    if (!all_dates_in_cosdata & !posix_cosdata) {
      stop("Something is wrong :( \n
           The 5 cosero date columns and the POSIXct colum could not be found")
    } else if (all_dates_in_cosdata & !posix_cosdata) {
      cos_data <- implode_cosdate(cos_data) # see: sub-chapter Implode date
    } else if (!all_dates_in_cosdata & posix_cosdata) {
      stop("POSIXct to COSdates not yet supported :(")
    }
    return(cos_data)
  }

  # ---------------------------------------------------------------------------
  implode_cosdate <- function(cos_data) {
    # pre: ====================================================================
    require("magrittr", quietly = TRUE)
    require("pasta", quietly = TRUE)
    assert_dataframe(cos_data)
    name_string <- cos_data %>% names(.) %>% tolower(.)
    # create posix_date column: ===============================================
    month_digits  <- sprintf("%02d",cos_data[[viscos_options("name_COSmonth")]])
    day_digits    <- sprintf("%02d",cos_data[[viscos_options("name_COSday")]])
    hour_digits   <- sprintf("%02d",cos_data[[viscos_options("name_COShour")]])
    minute_digits <- sprintf("%02d",cos_data[[viscos_options("name_COSmin")]])
    posix_date <- cos_data[[viscos_options("name_COSyear")]] %&%
        month_digits %&%
        day_digits %&%
        hour_digits %&%
        minute_digits %>%
      as.POSIXct(format = "%Y%m%d%H%M",
                 origin = .[1],
                 scale = "hourly",
                 tz = "UTC")
    cos_data[[viscos_options("name_COSposix")]] <- posix_date
    return(cos_data)
  }

  # ---------------------------------------------------------------------------
  # remove leading zeros from the names of cos_data (data.frame)
  remove_leading_zeros <- function(cos_data) {
    # pre: ====================================================================
    require("magrittr", quietly = TRUE)
    require("pasta", quietly = TRUE)
    cos_data %<>% remove_junk
    name_o <- viscos_options("name_o")
    search_o_or_s <- paste0(name_o,"|", viscos_options("name_s"))
    runoff_names <- cos_data %>% names(.)
    runoff_lowercase_names <- runoff_names %>% tolower(.)
    del_leading_zeros <- function(string) sub("^[0]+", "",string)
    # calc: ===================================================================
    idx_o <- grep(name_o , runoff_lowercase_names)
    separator <- runoff_lowercase_names %>%
      extract( idx_o[1] ) %>%
      gsub(name_o, "", .) %>%
      gsub("\\d", "", .)
    runoff_nums <- runoff_lowercase_names %>%
      gsub(search_o_or_s, "",.) %>%
      gsub(separator, "", .) %>%
      gsub("\\D", "", .)
    search_runoff_nums <- "[" %&% paste(runoff_nums, collapse = "") %&% "]"
    runoff_only_names <- runoff_names %>%
      gsub(search_runoff_nums, "", .) %>%
      gsub(separator,"",.)
    # clean up: ===============================================================
    runoff_new_numbers <- del_leading_zeros(runoff_nums)
    new_names <- runoff_new_numbers %>%
      gsub("\\d+", separator,.) %>%
      paste0(runoff_only_names, ., runoff_new_numbers)
    names(cos_data) <- new_names
    return(cos_data)
  }

  # ---------------------------------------------------------------------------
  #' Mark Periods
  #'
  #' Compute/Mark the periods within cos_data. The marking uses a monthly
  #' resolution, which are defined by the integers `start_month` and
  #' `end_month`.  
  #'
  #' @param cos_data a data.frame that contains the runoff information.
  #' @return  `cos_data` with an aditonal column with the marked periods.
  #'
  #' @import dplyr
  #' @import magrittr
  #'
  #' @export
  mark_periods <- function(cos_data, start_month = 10, end_month = 9) {
    # pre: ====================================================================
    assert_dataframe(cos_data)
    name_year <- viscos_options("name_COSyear")
    name_month <- viscos_options("name_COSmonth")
    cos_data %<>% remove_junk %>% complete_dates()
    eval_diff <- function(a) {c( a[1],diff(a) )}
    period_correction <- function(cos_data,period) {
      # tests:
      year_is_max <- cos_data[[name_year]] == max_year
      month_after_end <- cos_data[[name_month]] > end_month
      # assigmnet:
      ifelse((year_is_max & month_after_end), 0, period)
    }
    # calc: ===================================================================
    # (I) get labels for the months: ##########################################
    if (start_month <= end_month) {
      period_range <- seq(start_month,end_month)
      out_of_period <- seq(1,12) %>% extract( !(seq(1,12) %in% period_range) )
    } else if (start_month > end_month) {
      range_1 <- seq(start_month,12)
      range_2 <- seq(1,end_month)
      period_range <- c(range_1,range_2)
      out_of_period <- seq(1,12) %>% extract( !(seq(1,12) %in% period_range) )
    }
    # (II) mark periods: ######################################################
    start_months_in_data <- cos_data[[name_month]] %in% c(start_month)
    cos_data[[viscos_options("name_COSperiod")]] <- start_months_in_data %>%
      eval_diff(.) %>%
      pmax(.,0) %>%
      cumsum(.)
    out_period_in_data <- cos_data[[name_month]] %in% out_of_period
    cos_data$period[out_period_in_data] <- 0
    # (III) corrections for last year #########################################
    max_year <- max(cos_data[[name_year]])
    marked_cos_data <- dplyr::mutate(cos_data,
                              period = period_correction(cos_data, period)
      )
    return(marked_cos_data)
  }

  # ---------------------------------------------------------------------------
  #' Convert cos_data to xts-format
  #'
  #' Converts the cos_data (class: data_frame) into an xts object
  #'
  #' @return xts object of the cos_data data.frame
  #' @import zoo
  #' @importFrom xts xts
  #' @import magrittr
  cos_data_as_xts <- function(cos_data) {
    # pre: ====================================================================
    assert_dataframe(cos_data)
    assert_junk(cos_data)
    assert_complete_date(cos_data)
    # calc: ===================================================================
    # set every- name to lover capitals and generate xts frame
    new_names <- cos_data %>% names(.) %>% tolower(.)
    name_posix <- viscos_options("name_COSposix") %>% tolower(.)
    cos_data <- cos_data %>%
      remove_leading_zeros(.) %>%
      magrittr::set_names(new_names)
    cos_data_as_xts <- xts(x = cos_data[], order.by = cos_data[[name_posix]])
    return(cos_data_as_xts)
  }

  #' Loading many libraries at once
  #' @param x character vector. Name(s) of the libraries that are loaded/installed.
  #' @param ... Arguments passed to \code{\link{require}} and \code{\link{install.packages}}
  #' @author Simon Frey
  #' @description This function tries to load more than one package at once. If any of these packages is not installed it tries to istall them and load them afterward.
  #' @export
  #' @examples
  #' # loading xts 
  #' libraries("xts")
  #' libraries(c("xts","shiny"))
  #' @return Returns nothing but gives a warning if it cannot load/install a library/package
  #' @seealso \code{\link{require}}, \code{\link{library}}, \code{\link{install.packages}} 
  
 libraries <- function(x, ...){
  
  temp <- suppressWarnings(unlist(lapply(x,require,character.only=TRUE, ...)))
  
  if(any(!temp)){
    w <- which(!temp)
    install.packages(x[w],...)

    
    temp <- suppressWarnings(unlist(lapply(x[w],require,character.only=TRUE, ...)))
    if(!any(temp)){
      w <- which(!temp)
      stop(paste("Error loading ",x[w],sep=""))
    }
  }
 }
