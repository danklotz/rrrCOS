library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {# executes calculation file

  xts_slctd_data <-  reactive({
    select(d_xts,matches(d_raw_names[ as.integer(input$basin_num)*3 - 2 ]),matches(d_raw_names[ as.integer(input$basin_num)*3 ] )) %>%
    select(., Qobs = matches( d_raw_names[ as.integer(input$basin_num)*3 - 2 ]),
             Qsim = matches( d_raw_names[ as.integer(input$basin_num)*3])) %>%
    xts(.,order.by = d_xts$POSIXdate)
  })

  xts_slctd_error <- reactive({
    select(d_xts,matches(d_raw_names[ as.integer(input$basin_num)*3 - 2 ]),matches(d_raw_names[ as.integer(input$basin_num)*3 ] )) %>%
    select(., Qobs = matches( d_raw_names[ as.integer(input$basin_num)*3 - 2 ]),
             Qsim = matches( d_raw_names[ as.integer(input$basin_num)*3])) %>%
    mutate(.,error = Qsim-Qobs) %>% 
    select(.,error) %>%
    xts(.,order.by = d_xts$POSIXdate)
  })
   bsns_obs <- reactive({
     d_raw_names[(input$basin_num*3)-2]
   })
  
  bsns_sim <- reactive({
    d_raw_names[input$basin_num*3]
  })
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
  # output pltos
  output$NSE <- renderPlot({
    grid.arrange(plt_ynse,plt_tnse,ncol = 2,widths = c(3/4, 1/4))
  })
  output$KGE <- renderPlot({
    grid.arrange(plt_ykge,plt_tkge,ncol = 2,widths = c(3/4, 1/4))
  })
  output$pbias <- renderPlot({
    grid.arrange(plt_ypbias,plt_tpbias,ncol = 2,widths = c(3/4, 1/4))
  })
  output$corr <- renderPlot({
    grid.arrange(plt_ycor,plt_tcor,ncol = 2,widths = c(3/4, 1/4))
  })
  
})