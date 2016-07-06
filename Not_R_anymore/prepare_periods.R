#' calculate periods
#' 
#' Mark the periods within runoff_data. The range of the period is defined monthly by the integers start_month and end_month.  
#' @param runoff_data The data.frame, which contains the runoff information
#' @return The runoff data.frame reduced and ordered according to the hydrological years within the data. 
#' \strong{Note:} The hydrological years are formatted as characters.
#' @export
prepare.periods <- function(runoff_data, start_month, end_month) {
  # pre 
    require(dplyr, quietly = TRUE)
    require(magrittr, quietly = TRUE)
    if ( !is.data.frame(runoff_data) ) stop("runoff_data is no data_frame!")
    runoff_data %<>% prepare.complete_date()
  # calc:
  # get labels for the monts
  if (start_month <= end_month ) {
    period_range <- seq(start_month,end_month)
  } else if (start_month > end_month) {
    range_1 <- seq(start_month,12)
    range_2 <- seq(1,end_month)
    period_range <- c(range_1,range_2)
    out_of_period <- seq(1,12) %>% extract( !(seq(1,12) %in% period_range) )
  }
  # mark periods:
    eval_dif <- function(a) {c(a[1],diff(a))}
    runoff_data$period  <- runoff_data$mm %in% c(start_month) %>% eval_dif %>% pmax(.,0) %>% cumsum 
    runoff_data %<>% mutate(period = ifelse( ((yyyy == max(runoff_data$yyyy)) & (mm > end_month)),
                                               0,
                                               period) 
                            )
    runoff_data$period[runoff_data$mm %in% out_of_period] <- 0
    return(runoff_data)
    
  # old (not working) solution, to brag to christoph :)
#   start_sorter <- runoff_data$mm %in% c(start_month) %>% eval_dif %>% pmax(.,0)
#   end_sorter <- runoff_data$mm %in% c(end_month) %>% eval_dif %>% pmin(.,0)
#   test1 <- cumsum(start_sorter)
#   test2 <- end_sorter
#   test <- test1 + test2
#   
#   start_points <- which(start_sorter == 1) 
#   end_points <- which(end_sorter == -1)  
#   end_sorter[end_points[1] ] = 0
#   # :( 
#   if (length(end_points) == length(start_points)) {
#     test <- sum( start_points == end_points )
#     if (test == 3) {
#       start_points <- start_points[ 1:(length(start_points)-1) ]
#       end_points <- end_points[2:length(end_points)]
#     }
#     else if (length(end_points) > length(start_points)) {
#       end_points <- end_points[2:length(end_points)]
#     }
#   }
#   #
#   runoff_data$period <- 0
#   for ( k in 1:length(end_points) ) {
#     period_idx <- seq(start_points[k],end_points[k])
#     runoff_data$period %<>% replace(period_idx, k)
#   }
  # return(runoff_data)
}
