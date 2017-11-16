#' Objective Functions
#'
#' Objective Functions (Performance metrics) play an important role for 
#' hydrological model calibraiton. Therefore, `coscos` provides a set of innate
#' objective functions that can be used for model evaluation. The currently 
#' implemented functions are executed fortran code and the handling is inspired
#' by the `hydroGOF` package. 
#'
#' @useDynLib coscos
#' @param o The reference data or observations (o_data)
#' @param s The created data or the simulations (s_data)
#' @name objective_functions
NULL

# --------------------------------------------------------------------------
#' Nash-Sutcliffe Efficiency
#'
#' @rdname objective_functions
#' @importFrom purrr map2_dbl
#' @importFrom magrittr set_names
#' @export
of_nse <- function(o, s, na.rm = TRUE) {
  # pre sets:
  o <- as_tibble(o)
  s <- as_tibble(s)
  rows <- nrow(s) %>% as.integer(.)
  cols <- ncol(s) %>% as.integer(.)
  if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
  if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
  # computation:
  of_nse <- map2_dbl(o,
                     s,
                     function(x,y) of_wrapper(x, y, rows, d_name = "f_nse", na.rm = na.rm)) %>%
    set_names(paste("nse", 1:cols, sep = ""))
  return(of_nse)
}

# --------------------------------------------------------------------------
#' KGE 
#'
#' @rdname objective_functions
#' @export
of_kge <- function(o, s, na.rm = TRUE) {
  # pre sets:
  o <- as_tibble(o)
  s <- as_tibble(s)
  rows <- nrow(s) %>% as.integer(.)
  cols <- ncol(s) %>% as.integer(.)
  if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
  if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
  # computation:
  of_kge <- map2_dbl(o,
                     s,
                     function(x,y) of_wrapper(x, y, rows, na.rm = na.rm)
  ) %>%
    set_names(paste("kge", 1:cols, sep = ""))
  return(of_kge)
}
# --------------------------------------------------------------------------
#' Percentage Bias
#'
#' @rdname objective_functions
#'
#' @export
of_bias <- function(o, s, na.rm = TRUE) {
  # pre sets:
  o <- as_tibble(o)
  s <- as_tibble(s)
  rows <- nrow(s) %>% as.integer(.)
  cols <- ncol(s) %>% as.integer(.)
  if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
  if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
  # computation:
  of_pbias <- map2_dbl(o, s,
                       function(x,y) of_wrapper(x, y, rows, d_name = "f_bias", na.rm = na.rm)) %>%
    set_names(paste("bias", 1:cols, sep = ""))
  return(of_pbias)
}
# --------------------------------------------------------------------------
#' Percentage Bias
#'
#' @rdname objective_functions
#'
#' @export
of_pbias <- function(o, s, na.rm = TRUE) {
  # pre sets:
  o <- as_tibble(o)
  s <- as_tibble(s)
  rows <- nrow(s) %>% as.integer(.)
  cols <- ncol(s) %>% as.integer(.)
  if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
  if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
  # computation:
  of_pbias <- map2_dbl(o, s,
                       function(x,y) of_wrapper(x, y, rows, d_name = "f_pbias", na.rm = na.rm)) %>%
    set_names(paste("pbias", 1:cols, sep = ""))
  return(of_pbias)
}
# --------------------------------------------------------------------------
#' Correlation
#'
#' @rdname objective_functions
#' 
#' @export
of_cor <- function(o, s) {
  # pre sets:
  o <- as_tibble(o)
  s <- as_tibble(s)
  rows <- nrow(s) %>% as.integer(.)
  cols <- ncol(s) %>% as.integer(.)
  if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
  if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
  # computation:
  stats::cor(o, s) %>% diag(.)
}

# --------------------------------------------------------------------------
#' Mean Squared Error
#'
#' @rdname objective_functions
#'
#' @export
of_mse <- function(o, s, na.rm = TRUE) {
  # pre sets:
  o <- as_tibble(o)
  s <- as_tibble(s)
  rows <- nrow(s) %>% as.integer(.)
  cols <- ncol(s) %>% as.integer(.)
  if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
  if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
  # computation:
  of_mse <- map2_dbl(o, s,
                     function(x,y) of_wrapper(x, y, rows, d_name = "f_mse", na.rm = na.rm)) %>%
    set_names(paste("mse", 1:cols, sep = ""))
  return(of_mse)
}
# --------------------------------------------------------------------------
#' Root Mean Squared Error
#'
#' @rdname objective_functions
#'
#' @export
of_rmse <- function(o, s, na.rm = TRUE) {
  cols <- ncol(s) %>% as.integer(.)
  d_mse(o,s,na.rm) %>%
    sqrt(.) %>%
    set_names(paste("rmse", 1:cols, sep = "")) %>%
    return(.)
}

# --------------------------------------------------------------------------
#' Inverted Nash-Sutcliffe Efficiency
#'
#' @rdname objective_functions
#'
#' @export
of_inse <- function(o, s, na.rm = TRUE) {
  cols <- ncol(s) %>% as.integer(.)
  d_nse(o, s, na.rm = na.rm) %>%
    set_names(paste("inse", 1:cols, sep = "")) %>%
    return(.)
}

of_wrapper <- function(obs, sim, rows, ndstart = 1L, ndend = rows, d_name = "f_kge", na.rm) {
  idx_to_eval <- as.integer( 1L - (is.na(obs) + is.na(sim)) )
  na_count <- sum(idx_to_eval)
  if (!na.rm & (na_count > 0)) {
    stop("There are NAs in the data but `na.rm` is set to `FALSE`!")
  }
  out <- .Fortran(d_name,
                  XSIM = as.double(sim),
                  XOBS = as.double(obs),
                  maxday = rows,
                  NDSTART = 1L,
                  NDEND = rows,
                  EVAL = idx_to_eval,
                  of = as.double(-999.9),
                  NAOK = TRUE)
  return(out$of)
}
