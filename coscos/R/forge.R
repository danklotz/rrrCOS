#' Forge cosdata
#' 
#' Filter or change `cosdata` into an other format. 
#' 
#' @importFrom dplyr select starts_with
#' @importFrom tidyr gather
#' @import pasta
#' @export
forge <- function(cosdata, form = "tidy", opts = coscos::viscos_options()) {
  # pre sets:
  if (!is.character(form)) 
    stop("form must be a character!")
  le_data <- cook_cosdata(cosdata)
  # build different forms of data
  if(form == "tidy") {
    # standard tidy form: =================================================
    le_names <- names(le_data)
    grepl(pattern = opts$name_o %&% "|" %&% opts$name_s,
          x = le_names,
          ignore.case = TRUE) %>% 
      le_names[.] -> 
      le_q_names
    columns_2_remove <- c(opts$name_COSyear,
                          opts$name_COSmonth,
                          opts$name_COSday,
                          opts$name_COShour,
                          opts$name_COSmin)
    le_output <- select_(le_data, .dots = "-" %&% columns_2_remove) %>%
      gather_(.,
              key_col = c("key"),
              value_col = c("value"),
              gather_cols = c(le_q_names)
              )
  } else if (form == "o_data") {
    # cosdata with only o-columns =========================================
    le_names <- names(le_data)
    data_names <- grepl(opts$name_o %&% ".*",
                       le_names,
                       ignore.case = TRUE) %>% le_names[.]
    columns_2_keep <- c(opts$name_COSyear,
                        opts$name_COSmonth,
                        opts$name_COSday,
                        opts$name_COShour,
                        opts$name_COSmin, 
                        data_names,
                        opts$name_COSposix, 
                        opts$name_COSperiod)
    le_output <- select_(le_data, .dots = columns_2_keep)
  } else if (form == "s_data") {
    # cosdata with only s-columns =========================================
    le_names <- names(le_data)
    data_names <- grepl(opts$name_s %&% ".*",
                       le_names,
                       ignore.case = TRUE) %>% le_names[.]
    columns_2_keep <- c(opts$name_COSyear,
                        opts$name_COSmonth,
                        opts$name_COSday,
                        opts$name_COShour,
                        opts$name_COSmin, 
                        data_names,
                        opts$name_COSposix, 
                        opts$name_COSperiod)
    le_output <- select_(le_data, .dots = columns_2_keep)
  } else if (form == "o_only") {
    # only o-columns: ====================================================
    le_names <- names(le_data)
    data_names <- grepl(opts$name_o %&% ".*",
                        le_names,
                        ignore.case = TRUE) %>% le_names[.]
    le_output <- select_(le_data, .dots = data_names)
  } else if (form == "s_only") {
    # only s-columns: ====================================================
    le_names <- names(le_data)
    data_names <- grepl(opts$name_s %&% ".*",
                        le_names,
                        ignore.case = TRUE) %>% le_names[.]
    le_output <- select_(le_data, .dots = data_names)
  } else {
    stop("`form = " %&&% form %&%"` does not exist! See: ?forge")
  }
  return(le_output)
}

