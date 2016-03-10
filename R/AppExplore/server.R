require(shiny, quietly = TRUE)
require("xts")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {# executes calculation file
  # select the basin from the data
  #§ Problem: QOBS%_02 assumes a formatted integer format ! This should not be, Maybe try "stringr"
  
  # get strings used in the naming of runoff_data 
  just_words <- names(d_runoff) %>% gsub("\\d","",.) %>% unique
  obs_string <- just_words[ just_words %>% tolower %>% grep("qobs",.) ]
  sim_string <- just_words[ just_words %>% tolower %>% grep("qsim",.) ]
  # 
  '%&%' <- function(a,b) paste(a,b,sep = "")
  select_OBS <- reactive({ obs_string %&% input$basin_num %&% "$" })
  select_SIM <- reactive({ sim_string %&% input$basin_num %&% "$" })
  
  slctd_data <- reactive({
    select(d_runoff,
           matches( select_OBS() ),
           matches( select_SIM() )
           ) %>%
    select(Qobs = matches( select_OBS() ),
           Qsim = matches( select_SIM() )) 
  })
  # create xts-formated table for use in dygraphs
  xts_slctd_data <- reactive ({
    xts(slctd_data(),order.by = d_runoff$POSIXdate)
  })
  # get error of the basin
  xts_slctd_error <- reactive({
    slctd_data() %>% 
    mutate(error = Qsim-Qobs) %>% 
    select(error) %>%
    xts(order.by = d_runoff$POSIXdate)
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
      paste(strftime(slctd_from(), format = "%d %b %Y"),
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
      fetch_some_ofun( sub_slctd()$Qobs,sub_slctd()$Qsim )
  })
})