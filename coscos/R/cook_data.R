# --------------------------------------------------------------------------
# Code for cooking raw data
# authors: Daniel Klotz, Johannes Wesemann, Mathew Herrnegger
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# --------------------------------------------------------------------------

#' Cook cosdata 
#' 
#' Generates a \code{cosdata tibble} from given \code{raw_data}.
#' 
#' @family cooking functions
#' 
#' @import pasta
#' @import tibble
#' @export
cook_cosdata <- function(raw_data,
                         opts = coscos::viscos_options()) {
  if ( "cosdata" %in% class(raw_data)) {
    # if input is already cosdata, check and return it:
    check_cosdata(raw_data,opts)
    return(raw_data)
  } else {
    # transformation: 
    le_cos <- raw_data %>% 
      remove_junk(.) %>% 
      complete_dates(.) %>% 
      mark_periods(.) %>% 
      as_tibble(.)  
    check_cosdata(le_cos) # checks 
    # make pseudo class & return: 
    class(le_cos) <- append(class(le_cos),"cosdata")
    return(le_cos)
  }
}

# --------------------------------------------------------------------------

  #' remove not needed column in cosdata
  #'
  #' Removes all columns which are not foreseen (see: viscos_options) from
  #' runoff data
  #'
  #' @param cosdata The cosdata data.frame (see vignette for info)
  #' @return data.frame object without the chunk
  #' @export
  remove_junk <- function(cosdata, opts = coscos::viscos_options()) {
    build_tibble(cosdata) # see: defensive code
    # determine names of cosdata and get regex:
    names_in_data <- cosdata %>% names(.)
    regex_columns <- regex1_allcosdata(opts) 
    # get idx and clean data: ================================================
    idx <- grep(regex_columns,names_in_data, ignore.case = TRUE)
    clean_cosdata <- only_observed_basins(cosdata[ ,idx])
    return( clean_cosdata )
  }

# ---------------------------------------------------------------------------
  
  # remove basins without observations
  #
  # Removes basins without observation (-999/NA values) from the provided data.frame
  #
  # @param cosdata A raw cosdata data.frame, which may contains basins
  # without observations.
  # \strong{Note:} It is assumed that all available basins are simulated!
  # @return data.frame without the observation-free basins
  #
  # @import magrittr
  # @import pasta
  only_observed_basins <- function(cosdata) {
    # pre: ====================================================================
    require("magrittr")
    require("pasta")
    build_tibble(cosdata)
    missing_data_marker <- viscos_options("missing_data")
    # check for missing obs: ==================================================
    # set NA values to viscos_options("missing_data") and check if there are
    # cloumns wihtouth observervation:
    chosen_cols <- which( names(cosdata) != viscos_options("name_COSposix") )
    rows_with_na <- is.na(cosdata[ ,chosen_cols])
    data_wihtouth_posix <- cosdata[ ,chosen_cols]
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
        grepl(names(cosdata), ignore.case = TRUE) %>%
        not(.)
      data_only_observed <- cosdata[ ,data_selection]
    } else {
      data_only_observed <- cosdata
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
  #' @param cosdata The data.frame, which contains the runoff information
  #' @param name_cosyear string with the name of the `COSdate` year column
  #' @param name_posix string with the name of the POSIXct column
  #' @return The new runoff data.frame with the added data-format.
  #'
  #' @family cooking functions
  #'
  #' @import magrittr
  #'
  #' @export
  cook_dates <- function(cosdata) {
    # pre: ====================================================================
    build_tibble(cosdata)
    date_names <- unlist(viscos_options("name_COSyear",
                                        "name_COSmonth",
                                        "name_COSmonth",
                                        "name_COShour",
                                        "name_COSmin")
                        )
    # check dates: ============================================================
    # stop if non-logical expression are obtained
    all_dates_in_cosdata <- any( date_names %in% names(cosdata) )
    posix_cosdata <- any(viscos_options("name_COSposix") == names(cosdata))
    if ( !is.logical(all_dates_in_cosdata) | !is.logical(posix_cosdata) ) {
      stop("Something is wrong :( \n
            Some of the date-columns could not be processed!")
    }
    # execute function for the available format: ==============================
    if (!all_dates_in_cosdata & !posix_cosdata) {
      stop("Something is wrong :( \n
           The 5 cosero date columns and the POSIXct colum could not be found")
    } else if (all_dates_in_cosdata & !posix_cosdata) {
      cosdata <- implode_cosdate(cosdata) # see: sub-chapter Implode date
    } else if (!all_dates_in_cosdata & posix_cosdata) {
      stop("POSIXct to COSdates not yet supported :(")
    }
    return(cosdata)
  }

  # ---------------------------------------------------------------------------
  implode_cosdate <- function(cosdata) {
    # pre: ====================================================================
    require("magrittr", quietly = TRUE)
    require("pasta", quietly = TRUE)
    build_tibble(cosdata)
    name_string <- cosdata %>% names(.) %>% tolower(.)
    # create posix_date column: ===============================================
    month_digits  <- sprintf("%02d",cosdata[[viscos_options("name_COSmonth")]])
    day_digits    <- sprintf("%02d",cosdata[[viscos_options("name_COSday")]])
    hour_digits   <- sprintf("%02d",cosdata[[viscos_options("name_COShour")]])
    minute_digits <- sprintf("%02d",cosdata[[viscos_options("name_COSmin")]])
    posix_date <- cosdata[[viscos_options("name_COSyear")]] %&%
        month_digits %&%
        day_digits %&%
        hour_digits %&%
        minute_digits %>%
      as.POSIXct(format = "%Y%m%d%H%M",
                 origin = .[1],
                 scale = "hourly",
                 tz = "UTC")
    cosdata[[viscos_options("name_COSposix")]] <- posix_date
    return(cosdata)
  }

  # ---------------------------------------------------------------------------
  # remove leading zeros from the names of cosdata (data.frame)
  remove_leading_zeros <- function(cosdata) {
    # pre: ====================================================================
    require("magrittr", quietly = TRUE)
    require("pasta", quietly = TRUE)
    cosdata %<>% remove_junk
    name_o <- viscos_options("name_o")
    search_o_or_s <- paste0(name_o,"|", viscos_options("name_s"))
    runoff_names <- cosdata %>% names(.)
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
    names(cosdata) <- new_names
    return(cosdata)
  }

  # ---------------------------------------------------------------------------
  #' Mark Periods
  #'
  #' Compute/Mark the periods within cosdata. The marking uses a monthly
  #' resolution, which are defined by the integers `start_month` and
  #' `end_month`.  
  #'
  #' @param cosdata a data.frame that contains the runoff information.
  #' @return `cosdata` with an additonal column with the marked periods.
  #'
  #' @import dplyr
  #' @import magrittr
  #'
  #' @export
  mark_periods <- function(cosdata, start_month = 10L, end_month = 9L) {
    # pre: ====================================================================
    as.integer(start_month)
    as.integer(end_month)
    name_year <- viscos_options("name_COSyear")
    name_month <- viscos_options("name_COSmonth")
    #
    eval_diff <- function(a) {c( a[1L],diff(a) )}
    period_correction <- function(cosdata,period) {
      # tests:
      year_is_max <- cosdata[[name_year]] == max_year
      month_after_end <- cosdata[[name_month]] > end_month
      # assigmnet:
      ifelse((year_is_max & month_after_end), 0L, period)
    }
    
    # calc: ===================================================================
    # get labels for the months: 
    if (start_month <= end_month) {
      period_range <- seq(start_month,end_month)
      out_of_period <- seq(1L,12L) %>% extract( !(seq(1,12) %in% period_range) )
    } else if (start_month > end_month) {
      range_1 <- seq(start_month,12L)
      range_2 <- seq(1L,end_month)
      period_range <- c(range_1,range_2)
      out_of_period <- seq(1L,12L) %>% extract( !(seq(1L,12L) %in% period_range) )
    }
    # mark periods: 
    start_months_in_data <- as.integer(cosdata[[name_month]] %in% c(start_month))
    cosdata[[viscos_options("name_COSperiod")]] <- start_months_in_data %>%
      eval_diff(.) %>%
      pmax(.,0L) %>%
      cumsum(.)
    out_period_in_data <- cosdata[[name_month]] %in% out_of_period
    cosdata$period[out_period_in_data] <- 0L
    # corrections for last year 
    max_year <- max(cosdata[[name_year]])
    marked_cosdata <- dplyr::mutate(cosdata,
                              period = period_correction(cosdata, period)
      )
    return(marked_cosdata)
  }

# ---------------------------------------------------------------------------
#' Convert cosdata to xts-format
#'
#' Converts the cosdata (class: data_frame) into an xts object
#' @return xts object of the cosdata data.frame
#' 
#' @keywords internal
#' 
#' @importFrom xts xts
#' @import zoo
#' @import magrittr
cosdata_as_xts <- function(cosdata, opts = coscos::viscos_options()) {
  # pre: ====================================================================
  assert_junk(cosdata)
  build_tibble(cosdata)
  assert_complete_date(cosdata)
  # calc: ===================================================================
  # set every- name to lover capitals and generate xts frame
  new_names <- cosdata %>% names(.) %>% tolower(.)
  name_posix <- viscos_options("name_COSposix") %>% tolower(.)
  cosdata <- cosdata %>%
    remove_leading_zeros(.) %>%
    magrittr::set_names(new_names)
  cosdata_as_xts <- xts(x = cosdata[], order.by = cosdata[[name_posix]])
  return(cosdata_as_xts)
}

get_basin_numbers <- function(cos_data) {
  build_tibble(cos_data)
  assert_junk(cos_data)
  #
  d_names <- names(cos_data)
  d_nums <- d_names  %>% gsub('\\D','',.) %>% unique
  d_nums <- d_nums[!(d_nums == "")] %>% as.integer
  return(d_nums)
}
