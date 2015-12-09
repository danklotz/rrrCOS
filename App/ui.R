# Define UI for application that draws a histogram
library(shinyFiles)
library(dygraphs)

shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    tags$style(type="text/css", "body {padding-top: 50px;}"),
    # header
    h1(img(src="icon_cosvis.png", height = 210, width = 210), align = "center"), 
    h6("v0.2 by Klotz, Wesemann & Herrnegger", align = "center"),
    tags$hr(),  
  navbarPage(title = "Navigation:",
    tabPanel("data_load",
      tags$p('Choose the App folder:'),
      shinyDirButton('APPfolder', 
        label='App Folder', 
        title='Please select the App folder'
      ),
      tags$p(),
      tags$p('Choose the COSEROreg folder:'),
      shinyDirButton('COSfolder', 
                     label='COSEROreg folder', 
                     title='Please select the COSEROreg folder'
                     ),
      tags$p(),
      actionButton("goButton", "Go!")
      ),
    tabPanel("runoff",
      # runoff 
      h3("<runoff check>"),
      selectInput("basin_num",
                    "# basins:",
                    choices = d_nums, 
                    selected = 1, 
                    width = "100px"),
      dygraphOutput("dygrph1", width = "100%", height = "400px"),
      dygraphOutput("dygrph2", width = "100%", height = "100px")
    ),
    navbarMenu("stats",
      tabPanel("Summary Tables", 
        tabsetPanel(tabPanel("-"),
                    tabPanel("NSE",plotOutput("NSE", width = "1050px", height = "1500px")),
                    tabPanel("KGE",plotOutput("KGE", width = "1050px", height = "1500px")),
                    tabPanel("pBIAS",plotOutput("pbias", width = "1050px", height = "1500px")),
                    tabPanel("CORR",plotOutput("corr", width = "1050px", height = "1500px")),
                    type = "pills"
                    )
      ),
      tabPanel("splitted graphs",
        tabsetPanel(tabPanel("-"),
                    tabPanel("NSE",includeHTML("www/expnd_nse.html")),
                    tabPanel("KGE",includeHTML("www/expnd_kge.html")),
                    tabPanel("pBIAS",includeHTML("www/expnd_pbias.html")),
                    tabPanel("CORR",includeHTML("www/expnd_cor.html")),
                    type=  "pills"
                    )
      )
    ),
  position = "fixed-top")
))