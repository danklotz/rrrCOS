#' Get basic objective function for runoff_data
#'
#' Calculate basic objective functions
#'(NSE, KGE, percentage BIAS, Correlation (see: xxx)) for
#' every basin and the chosen periods
#'
#' @param runoff_data runoff_data data.frame (see:xxx).
#' @return list of basic objective function evaluated for the different
#' hydrological years and over the whole timespan.
#' @export
extract_objective_functions <- function(runoff_data) {
  require("hydroGOF", quietly = TRUE)
  require("dplyr", quietly = TRUE)
  assert_dataframe(runoff_data)
  stopifnot( exists(viscos_options()$name_COSperiod, where = runoff_data) )
  # (I) reduce necessary computation
  evaluation_data <- runoff_data[
      runoff_data[[viscos_options()$name_COSperiod]] > 0,
    ]
  # (II) get information
  number_of_basins <- evaluation_data %>%
    names %>%
    unique %>%
    tolower %>%
    grepl(viscos_options()$name_data1 , .) %>%
    sum
  periods_in_data <- evaluation_data[[viscos_options()$name_COSperiod]] %>%
    unique
  number_of_periods <- periods_in_data %>% length
  # (III) calculate overall objective functions
  obj_fun  <- list()
  temp_x <- dplyr::select(evaluation_data,starts_with(viscos_options()$name_data1)) %>%
    unname
  temp_y <- dplyr::select(evaluation_data,starts_with(viscos_options()$name_data2)) %>%
    unname
  obj_fun$NSE <- hydroGOF::NSE(temp_y,temp_x)
  obj_fun$KGE <- hydroGOF::KGE(temp_y,temp_x)
  obj_fun$pBIAS <- hydroGOF::pbias(temp_y,temp_x)
  obj_fun$CORR <- cor(temp_y,temp_x) %>% diag(.)
  # (IV) calulcated period-vise objective functions
    # pre allocation of periodic variables:
    obj_fun$NSE_period <- matrix(nrow = number_of_periods, ncol = as.integer(number_of_basins), data = NA)
    obj_fun$KGE_period <- obj_fun$NSE_period
    obj_fun$pBIAS_periods <- obj_fun$NSE_period
    obj_fun$CORR_period <- obj_fun$NSE_period
    # calculation loop
    for (k in 1:number_of_periods)
    {
      temp_x <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options()$name_data1)) %>%
        unname
      temp_y <- dplyr::filter(evaluation_data,period == periods_in_data[k]) %>%
        dplyr::select(.,starts_with(viscos_options()$name_data2)) %>%
        unname
      obj_fun$NSE_period[k,1:number_of_basins] <- hydroGOF::NSE(temp_y,temp_x)
      obj_fun$KGE_period[k,1:number_of_basins] <- hydroGOF::KGE(temp_y,temp_x)
      obj_fun$pBIAS_period[k,1:number_of_basins] <- hydroGOF::pbias(temp_y,temp_x)
      obj_fun$CORR_period[k,1:number_of_basins] <- cor(temp_y,temp_x) %>% diag(.)
    }
  #
  return(obj_fun)
}
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
  #$ this is all suboptimal! Maybe exploit the global function or something
  runoff_data %<>% remove_leading_zeros
  if ( !viscos_options( )$name_COSposix %in% names(runoff_data) ) {
    runoff_data %<>% prepare_complete_date()
  }
  runoff_data <<- runoff_data
  d_xts <<- runoff_as_xts(runoff_data)
  d_names_all<- names(d_xts)
  idx_names <- d_names_all %>% tolower %>% grepl("\\d" ,.)
  d_names <<- d_names_all[idx_names]
  d_nums <<- d_names %>% gsub("\\D","",.) %>% as.integer %>% unique

  server <- function(input, output, session) {# executes calculation file
    # get strings used in the naming of runoff_data
    unique_data_names <- names(runoff_data) %>% gsub("\\d","",.) %>% tolower %>% unique
    x_string <- unique_data_names[ unique_data_names %>% grep(viscos_options( )$name_data1,.) ]
    y_string <- unique_data_names[ unique_data_names  %>% grep(viscos_options( )$name_data2,.) ]
    #
    '%&%' <- function(a,b) paste(a,b,sep = "")
    selector_x <- reactive({ x_string %&% input$basin_num %&% "$" })
    selector_y <- reactive({ y_string %&% input$basin_num %&% "$" })
    selected_data <- reactive({
      select(runoff_data,
             matches( selector_x() ),
             matches( selector_y() )
      ) %>%
        select(Qobs = matches( selector_x() ),
               Qsim = matches( selector_y() ))
    })
    # create xts-formated table for use in dygraphs
    xts_selected_data <- reactive ({
      xts(selected_data(),order.by = runoff_data[[viscos_options()$name_COSposix]])
    })
    # plots
    output$hydrographs <- renderDygraph({
      dygraph(xts_selected_data(), group="A") %>%
        dySeries("Qobs", 
                 label = visCOS::viscos_options()$name_data1,
                 color = viscos_options()$color_data1 ) %>%
        dySeries("Qsim", 
                 label = visCOS::viscos_options()$name_data2,
                 color = viscos_options()$color_data2 ) %>%
        dyOptions(includeZero = TRUE) %>%
        dyRangeSelector(height = 20, strokeColor = "")
    })
    # stats
    slctd_from <- reactive({
      if (!is.null(input$hydrographs_date_window))
        input$hydrographs_date_window[[1]]
    })
    slctd_to <- reactive({
      if (!is.null(input$hydrographs_date_window))
        input$hydrographs_date_window[[2]]
    })
    # stats header
    output$slctd_info <- renderText({
      if (!is.null(input$hydrographs_date_window))
        paste(strftime(slctd_from(), format = "%d %b %Y"),
              "-",
              strftime(slctd_to(), format = "%d %b %Y"),
              sep = " ")
    })
    # stats calc
    sub_slctd <- reactive({
      if (!is.null(input$hydrographs_date_window))
        xts_selected_data()[paste(strftime(slctd_from(), format = "%Y-%m-%d-%H-%M"),
                               strftime(slctd_to(), format = "%Y-%m-%d-%H-%M"),
                               sep = "/")]
    })

    output$slctd_OF <- renderTable({
      if (!is.null(input$hydrographs_date_window))
        out <- serve_ofun( sub_slctd()$Qobs,sub_slctd()$Qsim )
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
          textOutput("slctd_info"),
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

