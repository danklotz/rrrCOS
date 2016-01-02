library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {# executes calculation file
  # select the basin from the data
  slctd_data <- reactive({
    select(d_xts,
           matches(d_raw_names[ as.integer(input$basin_num)*3 - 2 ]),
           matches(d_raw_names[ as.integer(input$basin_num)*3 ] )) %>%
    select(Qobs = matches( d_raw_names[ as.integer(input$basin_num)*3 - 2 ]),
           Qsim = matches( d_raw_names[ as.integer(input$basin_num)*3])) 
  })
  
  xts_slctd_data <- reactive ({
    xts(slctd_data(),order.by = d_xts$POSIXdate)
  })
  # get error of the basin
  xts_slctd_error <- reactive({
    slctd_data() %>% 
    mutate(error = Qsim-Qobs) %>% 
    select(error) %>%
    xts(order.by = d_xts$POSIXdate)
  })
  # plots
  output$dygrph1 <- renderDygraph({
      dygraph(xts_slctd_data(), group="A") %>%
        dySeries("Qobs", label="Qobs",color= "steelblue" ) %>%
        dySeries("Qsim", label="Qsim",color= "palevioletred" ) %>%
        dyOptions(includeZero = TRUE) %>%
        dyRangeSelector(height = 20, strokeColor = "")
      })
  output$dygrph2 <- renderDygraph({
       dygraph(xts_slctd_error(),group="A") %>%
       dySeries(label="Error",color="orange") %>%
       dyOptions(includeZero = TRUE) %>%
       dyLegend(show = "never", hideOnMouseOut = FALSE)
  })
  # stats
  slctd_from <- reactive({
    if (!is.null(input$dygrph1_date_window)) 
      input$dygrph1_date_window[[1]]
    })
  slctd_to <- reactive({
    if (!is.null(input$dygrph1_date_window)) 
      input$dygrph1_date_window[[2]]
  })
  # stats header
  output$slctd_info <- renderText({
    if (!is.null(input$dygrph1_date_window))
      paste(strftime(slctd_from(), format = "%d %b %Y),
            "-",
            strftime(slctd_to(), format = "%d %b %Y"),
            sep = " ")
  })
  # stats calc 
  sub_slctd <- reactive({
    if (!is.null(input$dygrph1_date_window))
      xts_slctd_data()[paste(strftime(slctd_from(), format = "%Y-%m-%d-%H-%M"),
                        strftime(slctd_to(), format = "%Y-%m-%d-%H-%M"),
                        sep = "/")]
  })

  output$slctd_OF <- renderTable({
    if (!is.null(input$dygrph1_date_window))
      fetch.hydOF( sub_slctd()$Qobs,sub_slctd()$Qsim )
  })
})