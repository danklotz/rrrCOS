# Define UI for application that draws a histogram
library(dygraphs)

shinyUI(
  fluidPage(
    theme = "bootstrap.css", # modified theme 
    # header specs: 
    h1(img(src="icon_cosvis.png", height = 210, width = 210), align = "center"), 
    h6("v0.2 by Klotz, Wesemann & Herrnegger", align = "center"),
    tags$hr(),  
    # runoff 
      h3("<runoff check>"),
      selectInput("basin_num",
                    "# basins:",
                    choices = d_nums, 
                    selected = 1, 
                    width = "100px"),
      dygraphOutput("dygrph1", width = "100%", height = "400px"),
      dygraphOutput("dygrph2", width = "100%", height = "100px"),
    h3("stats"),
    textOutput("slctd_info"),
    tableOutput("slctd_OF")
))