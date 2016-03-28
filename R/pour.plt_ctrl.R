#' get basic config for the plot control
#' 
#' Many of the serve function of visCOS are controlled via a list containing the different plotting controls. 
#' @export
#' @examples 
#' plt_ctrl <- pour_plt_ctrl
pour_plt_ctrl <- function( plot_title = "Title",
                            ltitle = "Legend", 
                            year_name = "Year", 
                            colors = c('#FF3300',
                                      '#f6f3a1',
                                      '#005900',
                                      "purple4"),
                            color_midpoint = 0.5, 
                            limits = c(0,1),
                            lb_cut = 0.0,
                            text_size = 0.5) {
  # def
    #§ missing :(
    if (length(colors) < 4) {
      stop("there must be 4 colors defined")
    }
  # calc
  plt_ctrl <- list() # reset list
  plt_ctrl$plot_title <- plot_title
  plt_ctrl$ltitle <- ltitle
  plt_ctrl$xlab <- year_name
  plt_ctrl$clr1 <- colors[1]
  plt_ctrl$clr2 <- colors[2]
  plt_ctrl$clr3 <- colors[3]
  plt_ctrl$clr4 <- colors[4]
  plt_ctrl$midpoint <- color_midpoint
  plt_ctrl$limits <- limits
  plt_ctrl$lb_cut <- lb_cut
  plt_ctrl$text_size <- text_size
  return(plt_ctrl)
}
