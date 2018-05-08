#' Map2 for cosdata 
#' 
#' A preset \code{fold} function that wraps around the \link[purrr]{map2}) function from \code{purrr} package, so that the provided function \code{.f} is applied in pairwise order to all \eqn{o} and \eqn{s} columns. The \link[purrr]{map2} function is not fixed; other mapping functions can be used by changing the \code{mapper}. 
#' 
#' @import pasta 
#' @importFrom purrr map2
#' @importFrom lazyeval expr_text
#' @importFrom stats setNames
#' @export
map2_cosdata <- function(cosdata, 
                         .f, 
                         mapper = purrr::map2) {
  if (missing(cosdata)) stop("cosdata is missing, pls define it.")
  if (missing(.f)) stop(".f is missig, pls define the function.")
  if (class(mapper) != "function") stop("mapper needs to be of class `function`!")
  le_data <- coscos::cook_cosdata(cosdata)
  # 
  le_o <- coscos::mold(le_data, form = "o_only")
  le_s <- coscos::mold(le_data, form = "s_only")
  le_fun_names <- paste(lazyeval::expr_text(.f), collapse = "_")
  le_result <- mapper(le_o, le_s, .f) %>% 
    as.list(.) %>% 
    stats::setNames(., le_fun_names %_% 1:length(.))
  return(le_result)
}