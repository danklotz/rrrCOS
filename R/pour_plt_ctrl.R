#' get basic config for the plot control
#' 
#' Many of the serve function of visCOS are controlled via a list containing the different plotting controls. This function provides such a list with the basic settings added. 
#' @export
viscos_options <- GlobalOptions::setGlobalOptions( 
  plot_title = "title",
  legend_title = "legend",
  xlab = "year",
  clr1 = '#FF3300',
  clr2 = '#f6f3a1',
  clr3 = '#005900',
  clr4 = "purple4",
  midpoint = 0.5,
  limits = c(0,1),
  lb_cut = 0.0,
  text_size = 0.5
)
