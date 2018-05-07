#' Shape-transformations for cosdata
#' 
#' @description 
#' \code{mould} allows to filter, tidy or re-arrange \code{cosdata} into 
#' new formats. 
#' @author Daniel Klotz
#' 
#' @param cosdata The strictly defined data format (\code{cosdata}) used 
#'      within \pkg{visCOS} (see: \code{\link{cook_cosdata}})
#' @param form A string that defines into which form the \code{cosdata} is "mouldd"
#'      (see: 'details')
#' @param opts The options for \pkg{visCOS} (see: \code{\link{viscos_options}}) 
#' 
#' @details
#' The mould function transforms  \code{cosdata} into a number of different 
#' forms. Currently the following forms (strings) can be used:
#' \itemize{
#'   \item \emph{tidy} ... Transfroms the \code{cosdata} into a tidy form.
#'   \item \emph{s_data} ... Filters the \eqn{o} columns out of \code{cosdata}.
#'   \item \emph{o_data} ... Filters the \eqn{s} columns out of \code{cosdata}.
#'   \item \emph{s_only} ... Extracts the \eqn{s} columns only.
#'   \item \emph{o_only} ... Extracts the \eqn{o} columns only.
#'   \item \emph{shell} ... Returns the \code{cosdata} withouth the 
#'                            data columns(i.e. the \eqn{o & s} columns).
#' }
#' 
#' @seealso \code{\link{cook_cosdata}}
#' @family cosdata manipulators
#' 
#' 
#' @examples
#' # tidy: 
#' mould( coscos::viscos_example() )
#'
#' # only s-data columns:
#' mould( coscos::viscos_example(), form = "s_only" )
#' 
#' @importFrom dplyr select starts_with
#' @importFrom tidyr gather_ gather
#' @import pasta
#' @export
mould <- function(cosdata, form = "tidy", opts = coscos::viscos_options()) {
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
      tidyr::gather_(.,
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
    stop("Sorry. `form = " %&&% form %&%"` does not exist! See: ?mould")
  }
  return(le_output)
}

