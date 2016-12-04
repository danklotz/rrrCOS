  #' explore runoff_data with Objective Functions
  #'
  #' Runs a Shiny App which can be used to get an overview of a runoff_data time
  #' series object.
  #'
  #' @param d_xts runoff_data formatted as time series
  #' 
  #' @import shiny 
  #' @import miniUI
  #' @importFrom xts xts
  #' @import dplyr
  #' @import magrittr
  #' @import dygraphs 
  #' @import hydroGOF
  #' @importFrom purrr map_df
  #' 
  #' @export
  #' 
  #' @examples
  #' # get example data,
  #' # explore the model performance
  #' d_runoff <- get_runoff_example()
  #' explore_runoff(d_runoff)
explore_runoff <- function(runoff_data,
                                   of_list = list(
                                     nse = of_nse, 
                                     kge = of_kge, 
                                     p_bias = of_pbias,
                                     r = of_cor
                                   ),
                                   start_date = NULL,
                                   end_date = NULL) {
  
  # pre-sets
  # (I) Defense
  if (is.null(names(of_list))){
    names(of_list) <- paste("of", 1:length(of_list), sep = "_")
  }
  clean_runoff_data <- runoff_data %>% remove_leading_zeros
  if ( !viscos_options("name_COSposix") %in% names(clean_runoff_data) ) {
    clean_runoff_data %<>% complete_dates
  }
  # (II)
  d_xts <- runoff_as_xts(clean_runoff_data)
  # (III)
  idx_names <- names(d_xts) %>%
    tolower %>% 
    grepl(viscos_options("name_o"),.)
  d_nums <- d_xts %>%
      names() %>%
      .[idx_names] %>%
      gsub("\\D","",.) %>%
      as.integer %>%
      unique
  server <- function(input, output, session) {
    # (I) get strings used in the naming of clean_runoff_data:
    unique_data_names <- names(clean_runoff_data) %>%
      gsub("\\d","",.) %>%
      tolower %>%
      unique
    x_string <- unique_data_names[ unique_data_names %>%
                                     grep(viscos_options("name_o"),.) ]
    y_string <- unique_data_names[ unique_data_names  %>%
                                     grep(viscos_options("name_s"),.) ]
    # (II) select data:
    '%&%' <- function(a,b) paste(a,b,sep = "") # %&% as substitute for function
    selector_x <- reactive({ x_string %&% input$basin_num %&% "$" }) # "$" terminates the searchstring; see regex
    selector_y <- reactive({ y_string %&% input$basin_num %&% "$" })
    selected_data <- reactive({
      select(clean_runoff_data,
             matches( selector_x() ),
             matches( selector_y() )
             ) %>%
        select(x = matches( selector_x() ),
               y = matches( selector_y() ))
    })
    # (III) create xts-formated table for use in dygraphs:
    xts_selected_data <- reactive ({
      xts(selected_data(),
          order.by = clean_runoff_data[[viscos_options("name_COSposix")]])
    })
    # (IV) create plots:
    output$hydrographs <- renderDygraph({
      dygraph( xts_selected_data() ) %>%
        dySeries("x",
                 label = visCOS::viscos_options("name_o"),
                 color = viscos_options("color_o")) %>%
        dySeries("y",
                 label = visCOS::viscos_options("name_s"),
                 color = viscos_options("color_s")) %>%
        dyRangeSelector(height = 20, strokeColor = "") %>% 
        dyCrosshair(direction = "vertical") %>%
        dyOptions(includeZero = TRUE) 
    })
    # (IV) get dygraph date bounds (switches):
    selcted_from <- reactive({
      if (!is.null(start_date)) {
        start_date
      } else if (!is.null(input$hydrographs_date_window)) {
        input$hydrographs_date_window[[1]]
      }
    })
    selcted_to <- reactive({
      if (!is.null(end_date)) {
        end_date
      } else if (!is.null(input$hydrographs_date_window)) {
        input$hydrographs_date_window[[2]]
      }

    })
    # (V) extract time_window for the stats header:
    output$selected_timewindow <- renderText({
      if (!is.null(input$hydrographs_date_window))
        paste(strftime(selcted_from(), format = "%d %b %Y"),
              "-",
              strftime(selcted_to(), format = "%d %b %Y"),
              sep = " ")
    })
    # (VI) calculate stats:
    sub_slctd <- reactive({
      if (!is.null(input$hydrographs_date_window))
        xts_selected_data()[paste(strftime(selcted_from(), format = "%Y-%m-%d-%H-%M"),
                               strftime(selcted_to(), format = "%Y-%m-%d-%H-%M"),
                               sep = "/")]
    })
    out_of <- reactive({
      if (!is.null(input$hydrographs_date_window)) {
          map_df(of_list, function(of_,x,y) of_(x,y), 
                 x = sub_slctd()$x, 
                 y = sub_slctd()$y ) #serve_of( sub_slctd()$x,sub_slctd()$y )
      }
    })
    
    output$slctd_OF <- renderTable(out_of())
    # (VII) exit when user clicks on done 
     # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- list(
        selected_time = c(strftime(selcted_from(), format = "%Y-%m-%d-%H-%M"),strftime(selcted_to(), format = "%Y-%m-%d-%H-%M")),
        selected_data = data.frame(date = index(sub_slctd()),
                                   coredata(sub_slctd())),
        selected_of = out_of()
      )
      stopApp(returnValue)
    })
  }
  ui <- miniPage(
    miniButtonBlock(selectInput("basin_num",
                                "# basin:",
                                choices = d_nums,
                                selected = 1, 
                                selectize = FALSE)),
    miniContentPanel(
      fillCol(
        flex = c(4,1),
        dygraphOutput("hydrographs", width = "100%", height = "100%"),
        fillCol(
          align = "center",
          textOutput("selected_timewindow"),
          tableOutput("slctd_OF")
        )
      )
    ),
    gadgetTitleBar("test")
  )
dyCrosshair <- function(dygraph, 
                        direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("examples/plugins/crosshair.js", 
                       package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}
  runGadget(ui,server)
}
