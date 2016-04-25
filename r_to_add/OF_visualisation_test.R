
# gonna test the idea with interactive objective function visuals ----------
require(magrittr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(shiny, quietly = TRUE)
require(miniUI, quietly = TRUE)
require(VARS, quietly = TRUE)
require(plotly, quietly = TRUE)
#

z_fun <- function(grid_points, mean, sigma, rho) {
  require(dplyr, quietly = TRUE)
  z <- multi_norm(grid_points,mean,sigma,rho)
  return( data.frame(grid_points,z))
} 

plot_fun <- function(data_frame) {
  the_plot <- ggplot(data_frame, aes(x = par1, y = par2, z = z)) +
    geom_raster(aes(fill = z), interpolate = TRUE) + 
    scale_fill_gradient(low="palevioletred", high="#93db70") + 
    stat_contour(color = "white") + 
    theme(legend.position="none")
  return(the_plot)
}

test_gadget <- function(n) {
  ui <- miniPage(
    gadgetTitleBar("Test ofun"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      fillCol( flex = c(2, 1),
        plotOutput("plot", height = "100%"),
        fillCol(
          sliderInput("z_axis", "z:", -2, 2, 0, step = 0.01, sep = "", animate = TRUE),
          fillRow(
            sliderInput("mean1", "mean1:", -1, 1, 0, step = 0.01, sep = ""),
            sliderInput("mean2", "mean2:", -1, 1, 0, step = 0.01, sep = ""),
            sliderInput("mean3", "mean3:", -1, 1, 0, step = 0.01, sep = "")
          ), 
          fillRow(
            sliderInput("sigma1", "sigma1:", 0, 2, 1, step = 0.01, sep = ""),
            sliderInput("sigma2", "sigma2:", 0, 2, 1, step = 0.01, sep = ""),
            sliderInput("sigma3", "sigma3:", 0, 2, 1, step = 0.01, sep = "")
          ), 
          fillRow(
            sliderInput("rho1", "rho1:", 0.001, 0.999, 0.1, step = 0.01, sep = ""),
            sliderInput("rho2", "rho2:", 0.001, 0.999, 0.1, step = 0.01, sep = ""),
            sliderInput("rho3", "rho3:", 0.001, 0.999, 0.1, step = 0.01, sep = "")
          )
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
    #
    set_mean <- reactive({
      c( as.numeric(input$mean1),as.numeric(input$mean2), as.numeric(input$mean3) )
    })
    set_sigma <- reactive({
      c( as.numeric(input$sigma1),as.numeric(input$sigma2), as.numeric(input$sigma3) )
    })
    set_rho <- reactive({
      c( as.numeric(input$rho1),as.numeric(input$rho2), as.numeric(input$rho3) )
    })
    #
    z_data <-  reactive({
      z_fun( grid_points(), mean = set_mean(), sigma = set_sigma(), rho = set_rho() )  
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




# test_gadget with plotly -------------------------------------------------

plot_fun2 <- function(data_frame) {
  the_plot <- plot_ly(data_frame, 
                      x = par1, 
                      y = par2, 
                      z = z,
                      color = z,
                      type = "scatter3d", 
                      mode = "markers",
                      aspectmode = "data")
  return(the_plot)
}

test_gadget2 <- function(n) {
  ui <- miniPage(
    gadgetTitleBar("Test ofun"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      fillCol( flex = c(2, 1),
               plotlyOutput("plot", height = "100%"),
               fillCol(
                 sliderInput("z_axis", "par3:", -2, 2, 0, step = 0.01, sep = "", animate = TRUE),
                 fillRow(
                   sliderInput("mean1", "mean1:", -1, 1, 0, step = 0.01, sep = ""),
                   sliderInput("mean2", "mean2:", -1, 1, 0, step = 0.01, sep = ""),
                   sliderInput("mean3", "mean3:", -1, 1, 0, step = 0.01, sep = "")
                 ), 
                 fillRow(
                   sliderInput("sigma1", "sigma1:", 0, 2, 1, step = 0.01, sep = ""),
                   sliderInput("sigma2", "sigma2:", 0, 2, 1, step = 0.01, sep = ""),
                   sliderInput("sigma3", "sigma3:", 0, 2, 1, step = 0.01, sep = "")
                 ), 
                 fillRow(
                   sliderInput("rho1", "rho1:", 0.001, 0.999, 0.1, step = 0.01, sep = ""),
                   sliderInput("rho2", "rho2:", 0.001, 0.999, 0.1, step = 0.01, sep = ""),
                   sliderInput("rho3", "rho3:", 0.001, 0.999, 0.1, step = 0.01, sep = "")
                 )
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
    #
    set_mean <- reactive({
      c( as.numeric(input$mean1),as.numeric(input$mean2), as.numeric(input$mean3) )
    })
    set_sigma <- reactive({
      c( as.numeric(input$sigma1),as.numeric(input$sigma2), as.numeric(input$sigma3) )
    })
    set_rho <- reactive({
      c( as.numeric(input$rho1),as.numeric(input$rho2), as.numeric(input$rho3) )
    })
    #
    z_data <-  reactive({
      z_fun( grid_points(), mean = set_mean(), sigma = set_sigma(), rho = set_rho() )  
    })
    output$plot <- renderPlotly({
      plot_fun2( z_data() )
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




# execution ---------------------------------------------------------------

test_gadget2(n = 30)


