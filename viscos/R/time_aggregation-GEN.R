#' Time Aggregation
#'
#' Aggregates \code{cosdata}) according to the
#' timely resolution defined via \code{aggregation}. Possible
#' resolution-choices are \code{'yyyy'} - year, \code{'mm'} - month and
#' \code{'dd'} - day and combinations thereof.
#'
#' @param cosdata Data-Format used within visCOS. A longer definition and explanation is provided in the introduction vignette; and the \code{coscos} function \pkg{cook_cosdata} provides a pre-set function to transform and check if your data is in the correct format).
#'
#' @import pasta
#' @importFrom tidyr gather_
#'
#' @export
aggregate_time <- function(cosdata,
                           key = "mm",
                           .funs = base::mean,
                           opts = coscos::viscos_options()) {
  if (class(key) != "character")
    stop("key must be a chracter. It currently is:" %&&% class(key))
  cos_data <- coscos::cook_cosdata(cosdata)
  le_aggr <- coscos::clump(cos_data, key = key, .funs = .funs)
  le_aggr[opts$name_COSposix] <- cos_data[[opts$name_COSposix]] %>%Ï
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
