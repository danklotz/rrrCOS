require(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = "bootstrap.css",
  # Application title
  titlePanel(img(src="icon_cosvis.png", height = 210, width = 210)), 
  h6("v0.2 by Klotz, Wesemann & Herrnegger"),
  # runoff 
  h3("<runoff check>"),
  selectInput("basin_num",
              "# basins:",
              choices = d_nums, 
              selected = 1, 
              width = "100px"),
  dygraphOutput("dygrph1", width = "100%", height = "400px"),
  dygraphOutput("dygrph2", width = "100%", height = "100px"),
  # of all in one:
  h3("<summary tables>"),
  tabsetPanel(tabPanel("-"),
              tabPanel("NSE",plotOutput("NSE", width = "1050px", height = "1500px")),
              tabPanel("KGE",plotOutput("KGE", width = "1050px", height = "1500px")),
              tabPanel("pBIAS",plotOutput("pbias", width = "1050px", height = "1500px")),
              tabPanel("CORR",plotOutput("corr", width = "1050px", height = "1500px")),
              type = "pills"
              ),
  # splitted graphs:
  h3("<separated graphs>"),
  tabsetPanel(tabPanel("-"),
              tabPanel("NSE",includeHTML("www/expnd_nse.html")),
              tabPanel("KGE",includeHTML("www/expnd_kge.html")),
              tabPanel("pBIAS",includeHTML("www/expnd_pbias.html")),
              tabPanel("CORR",includeHTML("www/expnd_cor.html")),
              type=  "pills"
              )
))