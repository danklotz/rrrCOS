#' water bilance plot
#' 
#' pltos the water bilance of a chosen basin
#' @export
serve.plot_waterbilance <- function(runoff_data, obs_name, calculate_mm = TRUE, cum_area_in_km = NULL) {
  plot(  serve.waterbilance(runoff_data, obs_name, calculate_mm, cum_area_in_km)  )
}
