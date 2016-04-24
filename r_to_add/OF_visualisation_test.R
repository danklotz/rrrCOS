
# gonna test the idea with interactive objective function visuals ----------
require(magrittr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(shiny, quietly = TRUE)
#
a <- seq(-20,20)
test <- expand.grid(p1 = a, p2 = a) %>% as.data.frame()
test$p3 <- 0.5
test$p4 <- 1

z_fun <- function(data_frame) {
  require(dplyr, quietly = TRUE)
  g <- data_frame
  z <- ( (1-g$p1^2) + (1-g$p2^2)*sin( (1:length(g$p2))*g$p3 ) ) * test$p4 
  return( cbind(g,z) )
} 
g <- z_fun(test)

plot_fun <- function(data_frame) {
  the_plot <- ggplot(data_frame, aes(x = p1, y = p2, z = z)) +
    geom_raster(aes(fill = z), interpolate = TRUE) + scale_fill_gradient(low="green", high="red") + 
    stat_contour()
  return(the_plot)
}

