
# gonna test the idea with interactive objective function visuals ----------
require(magrittr, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(shiny, quietly = TRUE)
#
a <- seq(-50,50)/10
test <- expand.grid(p1 = a, p2 = a) %>% as.data.frame()
test$p3 <- 0
test$p4 <- 1

z_fun <- function(data) {
  require(dplyr, quietly = TRUE)
  g <- data
  z <-  (1-g$p1^2)+5*sin( (1:length(g$p1))*g$p3 ) + (1-g$p2^2)+6*cos( (1:length(g$p2))*g$p4 )
  return( cbind(g,z) )
} 

plot_fun <- function(data_frame) {
  the_plot <- ggplot(data_frame, aes(x = p1, y = p2, z = z)) +
    geom_raster(aes(fill = z), interpolate = TRUE) + scale_fill_gradient(low="palevioletred", high="green") + 
    stat_contour()
  return(the_plot)
}

test_gadget <- function(data) {
  ui <- miniPage(
    gadgetTitleBar("Test ofun"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      fillCol( flex = c(3, 1),
        plotOutput("plot", height = "100%"),
        fillRow(
          sliderInput("in_p3", "p3", -1, 1, 0, step = 0.01, sep = "", animate = TRUE),
          sliderInput("in_p4", "p4", -1, 1, 0, step = 0.01, sep = "")
        )
      )
    )
  )
  server <- function(input, output, session) {
    updated_data <- reactive({ data.frame(p1 = data$p1,
                                          p2 = data$p2, 
                                          p3 = as.numeric(input$in_p3),
                                          p4 = as.numeric(input$in_p4))})
    z_data <-  reactive({  z_fun(updated_data()) })
    output$plot <- renderPlot({
      plot_fun(z_data())
    })
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(max(z_data()$z))
    })   
  }
  #
  runGadget(ui, server)
}  
test_gadget(test)

  


