
# gonna test the idea with interactive objective function visuals ----------
require(magrittr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(shiny, quietly = TRUE)
require(miniUI, quietly = TRUE)
require(VARS, quietly = TRUE)
#

z_fun <- function(grid_points, mean, sigma, rho) {
  require(dplyr, quietly = TRUE)
  z <- multi_norm(grid_points,mean,sigma,rho)
  return( data.frame(grid_points,z))
} 

plot_fun <- function(data_frame) {
  the_plot <- ggplot(data_frame, aes(x = par1, y = par2, z = z)) +
    geom_raster(aes(fill = z), interpolate = TRUE) + scale_fill_gradient(low="palevioletred", high="green") + 
    stat_contour(color = "white")
  return(the_plot)
}

test_gadget <- function(n, sigma, rho) {
  ui <- miniPage(
    gadgetTitleBar("Test ofun"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      fillCol( flex = c(4, 1),
        plotOutput("plot", height = "100%"),
        fillRow(
          sliderInput("z_axis", "z:", -2, 2, 0, step = 0.01, sep = "", animate = TRUE),
          sliderInput("mean1", "mean1:", -1, 1, 0.1, step = 0.01, sep = ""),
          sliderInput("mean2", "mean2:", -1, 1, 0.1, step = 0.01, sep = ""),
          sliderInput("mean3", "mean3:", -1, 1, 0.1, step = 0.01, sep = "")
        )
      )
    )
  )
  server <- function(input, output, session) {
    grid_points <- reactive({
      expand.grid(par1 = seq(-1, 1, length.out = n),
                  par2 = seq(-1, 1, length.out = n),
                  x3 = as.numeric(input$z_axis))
    })
    set_mean <- reactive({
      c( as.numeric(input$mean1),as.numeric(input$mean2), as.numeric(input$mean3) )
    })
    z_data <-  reactive({
      z_fun( grid_points(), mean = set_mean(), sigma, rho )  
    })
    output$plot <- renderPlot({
      plot_fun( z_data() )
      })
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp("yeah, yeah, yeah")
    })   
  }
  #
  runGadget(ui, server)
}  




# a <- seq(-1, 1, length.out = 30)
# grid_points <- expand.grid(x1 = a, x2 = a, x3 = a)
mean <- c(0.2,0.5,0)
sigma <- c(0.7,1,0.5)
rho <- c(0,0.3,0.9)
test_gadget(30, sigma, rho)

  


