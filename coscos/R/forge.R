#' Forge 
#' 
#' \code{forge} allows to filter, tidy or re-arrange \code{cosdata} into 
#' new formats. 
#' 
#' @param cosdata The strictly defined data format (\code{cosdata}) used 
#'      within \code{viscos} (see: \code{\link{cook_cosdata}})
#' @param form A string that defines into which form the \code{cosdata} is "forged"
#'      (see: 'details')
#' @param opts The options for viscos (see: \code{\link{viscos_options}}) 
#' 
#' @details
#' The forge functions transforms the \code{cosdata} tibble into a number of 
#' different forms. Currently the following froms can be used:
#' \itemize{
#'   \item \strong{tidy:} Transfroms the \code{cosdata} into a tidy form.
#'   \item \strong{s_data:} Filters the \eqn{o} columns out of \code{cosdata}.
#'   \item \strong{o_data:} Filters the \eqn{s} columns out of \code{cosdata}.
#'   \item \strong{s_only:} Extracts the \eqn{s} columns only.
#'   \item \strong{o_only:} Extracts the \eqn{o} columns only.
#'   \item \strong{shell:} Returns the \code{cosdata} withouth the 
#'                            data columns(i.e. the \eqn{o & s} columns).
#' }
#' 
#' @family cosdata manipulators
#' 
#' @examples
#' # tidy: 
#' forge( coscos::viscos_example() )
#'    
#' # only s-data columns:
#' forge( coscos::viscos_example(), form = "s_only" )
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
    le_output <- dplyr::select_(le_data, .dots = "-" %&% columns_2_remove) %>%
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
    le_output <- dplyr::select_(le_data, .dots = columns_2_keep)
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
    le_output <- dplyr::select_(le_data, .dots = columns_2_keep)
  } else if (form == "o_only") {
    # only o-columns: ====================================================
    le_names <- names(le_data)
    data_names <- grepl(opts$name_o %&% ".*",
                        le_names,
                        ignore.case = TRUE) %>% le_names[.]
    le_output <- dplyr::select_(le_data, .dots = data_names)
  } else if (form == "s_only") {
    # only s-columns: ====================================================
    le_names <- names(le_data)
    data_names <- grepl(opts$name_s %&% ".*",
                        le_names,
                        ignore.case = TRUE) %>% le_names[.]
    le_output <- dplyr::select_(le_data, .dots = data_names)
  } else if (form == "shell") {
    # remove o- and s-columns ============================================
    columns_2_keep <- c(opts$name_COSyear,
                        opts$name_COSmonth,
                        opts$name_COSday,
                        opts$name_COShour,
                        opts$name_COSmin, 
                        opts$name_COSposix, 
                        opts$name_COSperiod)
    le_output <- dplyr::select_(le_data, .dots = columns_2_keep)
  } else {
    stop("Sorry. `form = " %&&% form %&%"` does not exist! See: ?forge")
  }
  return(le_output)
}

