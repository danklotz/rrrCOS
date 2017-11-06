#' Distance Measures (Metrics)
#'
#' Different objective Functions, provided by visCOS. A detailed description
#' of each of the provided objective function is provided in the respective
#' vignette
#'
#' @useDynLib visCOS
#' @param o The reference data or observations (o_data)
#' @param s The created data or the simulations (s_data)
#' @name d_metrics
NULL

  # --------------------------------------------------------------------------
  #' Nash-Sutcliffe Efficiency
  #'
  #' @rdname d_metrics
  #' @importFrom coscos build_tibble
  #' @importFrom purrr map2_dbl
  #' @importFrom magrittr set_names
  #' @export
  d_nse <- function(o, s, na.rm = TRUE) {
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
                       function(x,y) d_wrapper(x, y, rows, d_name = "f_nse", na.rm = na.rm)) %>%
      set_names(paste("nse", 1:cols, sep = ""))
    return(of_nse)
  }

  # --------------------------------------------------------------------------
  #' KGE 2
  #'
  #' @rdname d_metrics
  #' @importFrom coscos build_tibble
  #' @export
  d_kge <- function(o, s, na.rm = TRUE) {
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
                       function(x,y) d_wrapper(x, y, rows, na.rm = na.rm)
      ) %>%
      set_names(paste("kge", 1:cols, sep = ""))
    return(of_kge)
  }
  # --------------------------------------------------------------------------
  #' Percentage Bias
  #'
  #' @rdname d_metrics
  #'
  #' @importFrom coscos build_tibble
  #'
  #' @export
  d_bias <- function(o, s, na.rm = TRUE) {
    # pre sets:
    o <- as_tibble(o)
    s <- as_tibble(s)
    rows <- nrow(s) %>% as.integer(.)
    cols <- ncol(s) %>% as.integer(.)
    if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
    if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
    # computation:
    of_pbias <- map2_dbl(o, s,
                       function(x,y) d_wrapper(x, y, rows, d_name = "f_bias", na.rm = na.rm)) %>%
      set_names(paste("bias", 1:cols, sep = ""))
    return(of_pbias)
  }
  # --------------------------------------------------------------------------
  #' Percentage Bias
  #'
  #' @rdname d_metrics
  #'
  #' @importFrom coscos build_tibble
  #'
  #' @export
  d_pbias <- function(o, s, na.rm = TRUE) {
    # pre sets:
    o <- as_tibble(o)
    s <- as_tibble(s)
    rows <- nrow(s) %>% as.integer(.)
    cols <- ncol(s) %>% as.integer(.)
    if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
    if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
    # computation:
    of_pbias <- map2_dbl(o, s,
                       function(x,y) d_wrapper(x, y, rows, d_name = "f_pbias", na.rm = na.rm)) %>%
      set_names(paste("pbias", 1:cols, sep = ""))
    return(of_pbias)
  }
  # --------------------------------------------------------------------------
  #' Correlation
  #'
  #' @rdname d_metrics
  #'
  #' @importFrom coscos build_tibble
  #'
  #' @export
  d_cor <- function(o, s) {
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
  #' @rdname d_metrics
  #'
  #' @importFrom coscos build_tibble
  #'
  #' @export
  d_mse <- function(o, s, na.rm = TRUE) {
    # pre sets:
    o <- as_tibble(o)
    s <- as_tibble(s)
    rows <- nrow(s) %>% as.integer(.)
    cols <- ncol(s) %>% as.integer(.)
    if(rows != nrow(o)) stop("o and s must have the same amount of rows (data points)")
    if(cols != ncol(o)) stop("o and s must have the same amount of columns (variables)")
    # computation:
    of_mse <- map2_dbl(o, s,
                       function(x,y) d_wrapper(x, y, rows, d_name = "f_mse", na.rm = na.rm)) %>%
      set_names(paste("mse", 1:cols, sep = ""))
    return(of_mse)
  }
  # --------------------------------------------------------------------------
  #' Root Mean Squared Error
  #'
  #' @rdname d_metrics
  #'
  #' @importFrom coscos build_tibble
  #'
  #' @export
  d_rmse <- function(o, s, na.rm = TRUE) {
    cols <- ncol(s) %>% as.integer(.)
    d_mse(o,s,na.rm) %>%
      sqrt(.) %>%
      set_names(paste("rmse", 1:cols, sep = "")) %>%
      return(.)
  }

  # --------------------------------------------------------------------------
  #' Inverted Nash-Sutcliffe Efficiency
  #'
  #' @rdname d_metrics
  #'
  #' @importFrom coscos build_tibble
  #'
  #' @export
  d_inse <- function(o, s, na.rm = TRUE) {
    cols <- ncol(s) %>% as.integer(.)
    d_nse(o, s, na.rm = na.rm) %>%
      set_names(paste("inse", 1:cols, sep = "")) %>%
      return(.)
  }

  d_wrapper <- function(obs, sim, rows, ndstart = 1L, ndend = rows, d_name = "f_kge", na.rm) {
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
