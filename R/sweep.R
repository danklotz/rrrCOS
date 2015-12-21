#' sweeps the filename away
#' 
#' Removes the filename from a path 
#' @param filepath path to a given file
#' @return path to the file
#' @importFrom magrittr %>%
#' @export
sweep.path <- function(filepath) {
  if (exists("filepath")) {
    sweep.path<- filepath %>%
      strsplit("\\\\") %>% .[[1]] %>% 
      strsplit("/") %>% .[[1]] %>% 
      .[1:(length(.)-1)] %>% 
      paste(.,collapse = "/") %>%
      paste(.,"/",sep="")
    return(sweep.path)
  } else { 
    stop("no filepath provided!")
#       sweep.path  <- file.choose() %>%
#         strsplit("\\\\") %>% .[[1]] %>% 
#         strsplit("/") %>% .[[1]] %>% 
#         .[1:(length(.)-1)] %>% 
#         paste(.,collapse = "/")
#       return(sweep.path)
    }
}

#' sweeps the basins withouth observation away
#' 
#' Removes basins withouth observation (-999/NA values) away from the provided dataframe 
#' @param d_AllBasins raw dataframe with 
#' @return data.frame without the observation-free basins
#' @export
sweep.NoSim <- function(d_AllBasins) {
  if (class(d_AllBasins) == "data.frame") {
    d_AllBasins[is.na(d_AllBasins)] = -999
    colmax <- function(x) lapply(X = d_AllBasins, FUN = max) # get max values of each column
    idx_temp <- which(colmax(d_AllBasins) == -999)
    idx_slct <- sort(c(idx_temp,idx_temp+1,idx_temp+2))
    d_onlyObserved <- d_AllBasins[-idx_slct]
    return(d_onlyObserved)
  } else {
    stop("Input Data must be a data.frame in the right format")
  }
}

#' transforms COSdate into a nice date format
#' 
#' Takes a data_frame containing the COSdate format and transforms it into a POSIX date series
#' @param data_frame A data.frame object containing a time series of the COSdate format
#' @return dates in the format of the POSIXct-class 
#' @export
implode.Cosdate <- function(data_frame) {
  POSIXdate <- paste(data_frame$yyyy,
                     sprintf("%02d",data_frame$mm),
                     sprintf("%02d",data_frame$dd),
                     sprintf("%02d",data_frame$hh),
                     sprintf("%02d",data_frame$min),
                     sep= "" ) %>% 
                 as.POSIXct(format = "%Y%m%d%H%M",tz="UTC")
  return(POSIXdate)
}
