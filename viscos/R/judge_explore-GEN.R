# -------------------------------------------------------------------------
#' Explore with Objective Functions
#'
#' Runs a Shiny Gadget which can be used to get an overview of a cosdata time 
#' series object.
#'
#' @import shiny
#' @import miniUI
#' @import xts 
#' @import dplyr
#' @import magrittr
#' @import dygraphs
#' @import pasta
#' @importFrom purrr map_df
#'
#' @export
#' 
#' @family judge functions
#' @rdname judge_explore
judge_explore <- function(cosdata,
                          .ofuns = list(nse   = coscos::of_nse,
                                            kge   = coscos::of_kge,
                                            pbias = coscos::of_pbias,
                                            corr  = coscos::of_cor),
                          opts =coscos::viscos_options()
                          ) {
  # (I) pre-sets: =========================================================
  name_o <- opts[["name_o"]]
  name_s <- opts[["name_s"]]
  name_lb <- opts[["name_lb"]]
  name_ub <-  opts[["name_ub"]]
  #
  if (is.null(names(of_metrics))){
    names(of_metrics) <- paste("of", 1:length(of_metrics), sep = "_")
  }
  cos_data <- cosdata %>% 
    coscos::cook_cosdata(.) %>% 
    coscos::remove_leading_zeros(.)
  #
  names_data <- names(cos_data) %>% tolower(.)
  number_lb <- grepl(name_lb,
                     names_data,
                     ignore.case = TRUE) %>% sum(.)
  number_ub <- grepl(name_ub,
                     names_data,
                     ignore.case = TRUE) %>% sum(.)
  plot_bounds <- FALSE
  if( (number_lb > 0) & (number_ub > 0)) {
    number_obs <- grepl(name_o,
                        names_data,
                        ignore.case = TRUE) %>% sum(.)
    number_sim <- grepl(name_s,
                        names_data,
                        ignore.case = TRUE) %>% sum(.)
    if (number_lb != number_ub) {
      stop("number of available bounds is not the same!" %&&%
             "#lb=" %&% number_lb %&&%
             "#ub=" %&% number_ub)
    } else if ((number_lb != number_obs) | (number_lb != number_sim)) {
      stop("Number of bounds is not the same as the o/s data!" %&&%
            "#bounds=" %&% number_lb %&&%
             "#obs=" %&% number_obs %&&%
             "#sim=" %&% number_sim)
    } else {
      plot_bounds <- TRUE # switch: plot bounds
    }
  }
  # basic stuff: =============================================================
  idx_names <- grepl(name_o,
                     names_data,
                     ignore.case = TRUE)
  d_nums <- names_data %>%
      .[idx_names] %>%
      gsub("\\D","",.) %>%
      as.integer(.) %>%
      unique(.)
  # (V) Define App: =========================================================
  server <- function(input, output, session) {
    # (a) get needed strings: ###############################################
    unique_data_names <- gsub("\\d","",names_data) %>%
      unique(.)
    x_string <- unique_data_names[ grep(name_o,
                                        unique_data_names) ]
    y_string <- unique_data_names[ grep(name_s,
                                        unique_data_names) ]
    if (plot_bounds) {
      lb_string <-  unique_data_names[ grep(name_lb,
                                            unique_data_names) ]
      ub_string <-  unique_data_names[ grep(name_ub,
                                            unique_data_names) ]
    }
    # (b) select data:
    # note: the regular expressions "$" terminates the searchstring
    selector_x <- reactive({ x_string %&% input$basin_num %&% "$" })
    selector_y <- reactive({ y_string %&% input$basin_num %&% "$" })
    selector_lb <- reactive({
      if(plot_bounds){
        lb_string %&% input$basin_num %&% "$"
      } else {
        NA
      }
    })
    selector_ub <- reactive({
      if(plot_bounds){
        ub_string %&% input$basin_num %&% "$"
      } else {
        NA
      }
    })
    selected_data <- reactive({
      if (plot_bounds) {
        cos_data %>%
          select(matches( selector_x() ),
                 matches( selector_y() ),
                 matches( selector_lb() ),
                 matches( selector_ub() )
                 ) %>%
          select(x = matches( selector_x() ),
                 y = matches( selector_y() ),
                 lb = matches( selector_lb() ),
                 ub = matches( selector_ub() ))
      } else {
        cos_data %>%
          select(matches( selector_x() ),
                 matches( selector_y() )
                 ) %>%
          select(x = matches( selector_x() ),
                 y = matches( selector_y() ))
      }
    })
    # (c) create xts-formated table for use in dygraphs:
    xts_selected_data <- reactive ({
      xts(selected_data(),
          order.by = cos_data[[ opts[["name_COSposix"]] ]])
    })
    # (d) create plots:
    base_graph <- reactive({
      if(plot_bounds) {
        dygraph( xts_selected_data() ) %>%
        dyAxis("y",
               label = opts[["data_unit"]]) %>%
        dySeries("x",
                 label = name_o,
                 color = opts[["color_o"]]) %>%
        dySeries("y",
                 label = name_s,
                 color = opts[["color_s"]]) %>%
        dySeries("lb",
                 label = name_lb,
                 color = "grey80") %>%
        dySeries("ub",
                 label = name_ub,
                 color = "grey80")
      } else {
        dygraph( xts_selected_data() ) %>%
        dyAxis("y",
               label = opts[["data_unit"]]) %>%
        dySeries("x",
                 label = name_o,
                 color = opts[["color_o"]]) %>%
        dySeries("y",
                 label = "name_s",
                 color = opts[["color_s"]])
      }
    })
    output$hydrographs <- renderDygraph({
      base_graph() %>%
        dyRangeSelector(height = 20, strokeColor = "") %>%
        dyCrosshair(direction = "vertical") %>%
        dyOptions(includeZero = TRUE,
                  retainDateWindow = TRUE,
                  animatedZooms = TRUE)
    })
    # (e) get dygraph date bounds (switches):
    selcted_from <- reactive({
      if (!is.null(input$hydrographs_date_window)) {
        input$hydrographs_date_window[[1]]
      }
    })
    selcted_to <- reactive({
      if (!is.null(input$hydrographs_date_window)) {
        input$hydrographs_date_window[[2]]
      }

    })
    # (f) extract time_window for the stats header:
    output$selected_timewindow <- renderText({
      if (!is.null(input$hydrographs_date_window))
        paste(strftime(selcted_from(), format = "%d %b %Y"),
              "-",
              strftime(selcted_to(), format = "%d %b %Y"),
              sep = " ")
    })
    # (g) calculate stats:
    sub_slctd <- reactive({
      if (!is.null(input$hydrographs_date_window))
        xts_selected_data()[paste(strftime(selcted_from(), format = "%Y-%m-%d-%H-%M"),
                               strftime(selcted_to(), format = "%Y-%m-%d-%H-%M"),
                               sep = "/")]
    })
    out_of <- reactive({
      if (!is.null(input$hydrographs_date_window)) {
          map_df(of_metrics, function(of_,x,y) of_(x,y),
                 x = sub_slctd()$x,
                 y = sub_slctd()$y ) #serve_of( sub_slctd()$x,sub_slctd()$y )
      }
    })

    output$slctd_OF <- renderTable(out_of())
    # (h) exit when user clicks on done
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
    path = system.file("plugins/crosshair.js",
                       package = "visCOS"),
    options = list(direction = match.arg(direction))
  )
}
  runGadget(ui,server)
}
