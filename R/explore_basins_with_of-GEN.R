#' explore runoff_data with Objective Funcitons
#'
#' Runs a Shiny App which can be used to get an overview of a runoff_data time
#' series object. 
#'
#' @param d_xts runoff_data formatted as time series
#' @export
#' @examples
#' # get example data,
#' # clean it and
#' # explore the model performance
#' d_runoff <- get_runoff_example()
#' explore_runoff_with_ofun(d_runoff)
explore_runoff_with_ofun <- function(runoff_data) {
  require("shiny", quietly = TRUE)
  require("dplyr", quietly = TRUE)
  require("magrittr", quietly = TRUE)
  require("xts", quietly = TRUE)
  require("dygraphs", quietly = TRUE)
  ##########################
  #$ this is all suboptimal! 
  # (I)
  clean_runoff_data <- runoff_data %>% remove_leading_zeros
  if ( !viscos_options( )$name_COSposix %in% names(clean_runoff_data) ) {
    clean_runoff_data %<>% prepare_complete_date
  }
  # (II)
    d_xts <- runoff_as_xts(clean_runoff_data)
    idx_names <- names(d_xts) %>% 
      grepl("\\d" ,.)
    d_nums <- d_xts %>% 
      names() %>%
      .[idx_names] %>% 
      gsub("\\D","",.) %>% 
      as.integer %>% 
      unique
  server <- function(input, output, session) {# executes calculation file
    # get strings used in the naming of clean_runoff_data
    unique_data_names <- names(clean_runoff_data) %>%
      gsub("\\d","",.) %>% 
      tolower %>%
      unique
    x_string <- unique_data_names[ unique_data_names %>% 
                                     grep(viscos_options( )$name_data1,.) ]
    y_string <- unique_data_names[ unique_data_names  %>% 
                                     grep(viscos_options( )$name_data2,.) ]
    # (II) select data
    '%&%' <- function(a,b) paste(a,b,sep = "")
    selector_x <- reactive({ x_string %&% input$basin_num %&% "$" })
    selector_y <- reactive({ y_string %&% input$basin_num %&% "$" })
    selected_data <- reactive({
      select(clean_runoff_data,
             matches( selector_x() ),
             matches( selector_y() )
      ) %>%
        select(x = matches( selector_x() ),
               y = matches( selector_y() ))
    })
    # create xts-formated table for use in dygraphs
    xts_selected_data <- reactive ({
      xts(selected_data(),
          order.by = clean_runoff_data[[viscos_options()$name_COSposix]])
    })
    # plots
    output$hydrographs <- renderDygraph({
      dygraph(xts_selected_data(), group="A") %>%
        dySeries("x", 
                 label = visCOS::viscos_options()$name_data1,
                 color = viscos_options()$color_data1 ) %>%
        dySeries("y", 
                 label = visCOS::viscos_options()$name_data2,
                 color = viscos_options()$color_data2 ) %>%
        dyOptions(includeZero = TRUE) %>%
        dyRangeSelector(height = 20, strokeColor = "")
    })
    # beginnning and end of time window
    selcted_from <- reactive({
      if (!is.null(input$hydrographs_date_window))
        input$hydrographs_date_window[[1]]
    })
    selcted_to <- reactive({
      if (!is.null(input$hydrographs_date_window))
        input$hydrographs_date_window[[2]]
    })
    # stats header
    output$selected_timewindow <- renderText({
      if (!is.null(input$hydrographs_date_window))
        paste(strftime(selcted_from(), format = "%d %b %Y"),
              "-",
              strftime(selcted_to(), format = "%d %b %Y"),
              sep = " ")
    })
    # stats calc
    sub_slctd <- reactive({
      if (!is.null(input$hydrographs_date_window))
        xts_selected_data()[paste(strftime(selcted_from(), format = "%Y-%m-%d-%H-%M"),
                               strftime(selcted_to(), format = "%Y-%m-%d-%H-%M"),
                               sep = "/")]
    })
    output$slctd_OF <- renderTable({
      if (!is.null(input$hydrographs_date_window))
        out <- serve_ofun( sub_slctd()$x,sub_slctd()$y )
    })
  }
  ui <- fluidPage(
      selectInput("basin_num",
                  "# basins:",
                  choices = d_nums,
                  selected = 1,
                  width = "100px"),
      dygraphOutput("hydrographs", width = "100%", height = "400px"),
      hr(),
      fluidRow(
        column(12, align = "center",
          textOutput("selected_timewindow"),
          tableOutput("slctd_OF")
        )
      )
    )
  shinyApp(ui,server)
}

# Get some objective functions (OF)
#
# Get some basic objective functions used in hydrology:
# Root Mean Squared Error, Correlation, NSE, KGE, pbias
# @return data.frame contianing basic OF
serve_ofun <- function(x,y) {
  require("hydroGOF", quietly = TRUE)
  require("magrittr", quietly = TRUE)
  # calc
  out <- data.frame(
    RMSE = rmse(y,x) %>% as.numeric,
    pbias = pbias(y,x) %>% as.numeric,
    NSE = NSE(y,x) %>% as.numeric,
    KGE = KGE(y,x) %>% as.numeric,
    corr = -cor(x,y) %>% diag(),
    beta =  mean(y)/mean(x),
    alpha =  sd(y)/sd(x)
  )
  return(out)
}

