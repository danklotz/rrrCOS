# ---------------------------------------------------------------------------
# Code for cooking data
# authors: Daniel Klotz, Johannes Wesemann, Mathew Herrnegger
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # --------------------------------------------------------------------------
  # rolling funcitons 
  roll_roll <- function(d1, 
                        d2 = NULL, 
                        fun = hydroGOF::NSE,
                        width,
                        ...) {
    # functions: =============================================================
    matrix_to_dataframe <- function(x) {
      if (is.matrix(x)) { x <- as.data.frame(x) }
      return(x)
    }
    funct_bi <- function(which, XX, YY, fun, ...) fun(XX[which], YY[which])
    replace_xvar_entries <- function(le_list,xvar) {
      le_list[names(xvar)] <- xvar
      return(le_list)
    }
    # calc: ==================================================================
    n <- length(d1)
    from <- pmax((1:n) - width + 1, 1)
    to <- 1:n
    elements <- apply(cbind(from, to), 1, function(x) seq(x[1],x[2])) %>% 
      matrix_to_dataframe(.) %>% 
      set_names(., paste(from, to, sep = ":") )
    skip <- sapply(elements, length) %>% 
      is_less_than(width)
    Xvar <- elements %>% 
      .[!skip] %>% 
      sapply(., function(e_r) funct_bi(e_r,d1,d2,fun))
    Xvar_final <- rep(new(class(Xvar[1]), NA), length(from)) %>% 
      set_names(elements) %>% 
      replace_xvar_entries(.,Xvar)
    return(Xvar_final)
  }

# --------------------------------------------------------------------------
#' Objectie Function Windowing 
#' 
#' Compute the objective function for a set of windows.
#' @export
  of_windows <- function(cos_data,
                       of,
                       lb = 0.0, 
                       min_window_size = 500,
                       max_window_size = 1000,
                       number_windows = 3, 
                       na_filling = FALSE) {
    length_data <- nrow(cos_data)
    data1 <- cos_data %>%
       select( starts_with(viscos_options("name_o")) )
    data2 <- cos_data %>%
       select( starts_with(viscos_options("name_s")) )
    data_numbers <- names(data1) %>%
      gsub(viscos_options("name_o"), "", ., ignore.case = TRUE) %>%
      gsub("\\D", "", ., ignore.case = TRUE)
  # make plotlist: =========================================================
    #§ care!!  we need to quarantee that step == 0 and window_size == 0 will 
    # $  not make problems!
    all_window_sizes <- seq(from = min_window_size,
                            to = max_window_size, 
                            length.out = number_windows) %>% 
      as.integer(.)
  # define functions for roll: =============================================
    fill_na_up <- function(in_data) {
      if (na_filling) {
        fill_idx <- in_data %>% 
          is.na(.) %>% 
          not(.) %>% 
          which(.) %>% 
          min(.)
        in_data[1:fill_idx] <- in_data[fill_idx]
      }
      return(in_data)
    }
    confine  <- function(in_data) {
      pmax(in_data,lb)
    }
    roll_of <- function(sub_data1, sub_data2) {
      sapply(all_window_sizes, 
             function(window_size) roll_roll(d1 = sub_data1,
                                             d2 = sub_data2,
                                             fun = of, 
                                             width = window_size)) %>%
        apply(., 2, fill_na_up) %>%
        apply(., 2, confine) %>% 
        as_tibble(.) %>% 
        set_names("of_window" %&% sprintf("%.3i",1:length(all_window_sizes))) %>% 
        dplyr::mutate(idx = 1:length_data,
                      posixdate = cos_data[[viscos_options("name_COSposix")]],
                      qobs = sub_data1, 
                      qsim = sub_data2) %>% 
        gather(., key, value, -idx, -posixdate) %>% 
        mutate(le_group = ifelse((key == "qobs" | key == "qsim"), "q", "of"))
    }
    # apply roll_of 
    window_list <- map2(data1,data2, roll_of)
  }

