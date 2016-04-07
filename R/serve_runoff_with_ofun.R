# Dive With OF ------------------------------------------------------------
#' serve with ofun 
#' 
#' Runs a Shiny App which can be used to get an overview of a runoff_data time series object. 
#' Explore the runoff_data with a little [shiny](http://shiny.rstudio.com/) App. 
#' 
#' @param d_xts runoff_data formatted as time series
#' @export
#' @examples 
#' # get example data, 
#' # clean it and 
#' # explore the model performance
#' d_runoff <- prepare.remove_chunk( pour_runoff_example() )
#' serve_runoff_with_ofun(d_runoff)
serve_runoff_with_ofun <- function(runoff_data) {
    # pre
    require("data.table", quietly = TRUE) 
    require("magrittr", quietly = TRUE)
    require("shiny", quietly = TRUE)
    require("xts", quietly = TRUE)
    require("dygraphs", quietly = TRUE)
  ##########################
  # calc
  #$ this is all suboptimal, maybe exploit the global function or something
  runoff_data %<>% prepare.names
  if ( !"POSIXdate" %in% names(runoff_data) ) {
    runoff_data %<>% prepare.complete_date()
  }
  runoff_data <<- runoff_data
  d_xts <<- prepare.runoff_as_xts(runoff_data)
  d_names_all<- names(d_xts)
  idx_names <- d_names_all %>% tolower %>% grepl("\\d" ,.)
  d_names <<- d_names_all[idx_names]
  d_nums <<- d_names %>% gsub("\\D","",.) %>% as.integer %>% unique 
  
  
  
  server <- function(input, output, session) {# executes calculation file
    # select the basin from the data
    #§ Problem: QOBS%_02 assumes a formatted integer format ! This should not be, Maybe try "stringr"
    
    # get strings used in the naming of runoff_data 
    just_words <- names(runoff_data) %>% gsub("\\d","",.) %>% unique
    obs_string <- just_words[ just_words %>% tolower %>% grep("qobs",.) ]
    sim_string <- just_words[ just_words %>% tolower %>% grep("qsim",.) ]
    # 
    '%&%' <- function(a,b) paste(a,b,sep = "")
    select_OBS <- reactive({ obs_string %&% input$basin_num %&% "$" })
    select_SIM <- reactive({ sim_string %&% input$basin_num %&% "$" })
    
    slctd_data <- reactive({
      select(runoff_data,
             matches( select_OBS() ),
             matches( select_SIM() )
      ) %>%
        select(Qobs = matches( select_OBS() ),
               Qsim = matches( select_SIM() )) 
    })
    # create xts-formated table for use in dygraphs
    xts_slctd_data <- reactive ({
      xts(slctd_data(),order.by = runoff_data$POSIXdate)
    })
    # get error of the basin
    xts_slctd_error <- reactive({
      slctd_data() %>% 
        mutate(error = Qsim-Qobs) %>% 
        select(error) %>%
        xts(order.by = runoff_data$POSIXdate)
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
        out <- serve_ofun( sub_slctd()$Qobs,sub_slctd()$Qsim )
        out
    })
  }
  #
  ui <- fluidPage(
      # runoff 
      h3("runoff:"),
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
    )
  

  shinyApp(ui,server)
}
















































#' # Dive With OF ------------------------------------------------------------
#' #' serve with ofun 
#' #' 
#' #' Runs a Shiny App which can be used to get an overview of a runoff_data time series object. 
#' #' Explore the runoff_data with a little [shiny](http://shiny.rstudio.com/) App. 
#' #' 
#' #' @param d_xts runoff_data formatted as time series
#' #' @export
#' #' @examples 
#' #' # get example data, 
#' #' # clean it and 
#' #' # explore the model performance
#' #' d_runoff <- prepare.remove_chunk( pour_runoff_example() )
#' #' serve_runoff_with_ofun(d_runoff)
#' serve_runoff_with_ofun <- function(runoff_data) {
#'   # pre
#'   require("data.table", quietly = TRUE) 
#'   require("magrittr", quietly = TRUE)
#'   require("shiny", quietly = TRUE)
#'   ##########################
#'   # calc
#'   #$ this is all suboptimal, maybe exploit the global function or something
#'   runoff_data %<>% prepare.names
#'   if ( !"POSIXdate" %in% names(runoff_data) ) {
#'     runoff_data %<>% prepare.complete_date()
#'   }
#'   runoff_data <<- runoff_data
#'   d_xts <<- prepare.runoff_as_xts(runoff_data)
#'   d_names_all<- names(d_xts)
#'   idx_names <- d_names_all %>% tolower %>% grepl("\\d" ,.)
#'   d_names <<- d_names_all[idx_names]
#'   d_nums <<- d_names %>% gsub("\\D","",.) %>% as.integer %>% unique 
#'   #
#'   runApp("R/AppExplore") #$ how do I fix the path to the app?
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
