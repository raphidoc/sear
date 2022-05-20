#' selection_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import lubridate

mod_selection_display_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(ns("Filters")),
    plotlyOutput(ns("Map"), width = NULL, height = 500)#,
    #DT::DTOutput(ns("DataTable"), width = NULL, height = 100)

  )
}

#' selection_display Server Functions
#'
#' @noRd
mod_selection_display_server <- function(id, Apla){

  stopifnot(is.reactive(Apla))

  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Add time filtering when the Apla reactive dataframe is created
    output$Filters <- renderUI({

      req(Apla())

      tagList(
        sliderInput(ns("TimeFilter"), "Time filter",
                    min = min(Apla()$DateTime), max = max(Apla()$DateTime),
                    value = c(min(Apla()$DateTime), max = max(Apla()$DateTime)),
                    timeFormat = "%T",
                    timezone = "+0000",
                    width = NULL,
                    step = 1),
        numericInput(ns("SpeedLimit"), "SpeedLimit", 4, step = 0.1)
      )


    })

    TimeInterval <- reactive({
      lubridate::interval(input$TimeFilter[1],input$TimeFilter[2])
    })

    SubUpApla <- reactiveVal({})

    observe({
      req(TimeInterval(), input$SpeedLimit)

      AplaTime <- UpApla() %>%
        filter(DateTime %within% TimeInterval() & Speed_N <= input$SpeedLimit)

      SubUpApla(AplaTime)
    })

    # Update tibble ObsType and ObsName on ApplyObs button click event
    UpApla <- reactiveVal({})

    observe({
      UpApla(Apla())
    })

    # Get the ID of plotly_mapbox selected point in: selected()$customdata
    Selected <- reactiveVal({})

    observeEvent(event_data("plotly_selected", source = "map"),{
      Selected(event_data("plotly_selected", source = "map")$customdata)
    })

    # Global map for the entire dataset ---------------------------------------

    output$Map <- renderPlotly({

      req(SubUpApla())

      ObsTypeColor <- c("Unknown" = "red", "Transit" = "black", "Transect" = "orange", "Station" = "green")

      zc <- zoom_center(SubUpApla()$Lat_DD, SubUpApla()$Lon_DD)
      zoom <- zc[[1]]
      center <- zc[[2]]

      p <- plot_mapbox(SubUpApla(),
                       lon = ~Lon_DD,
                       lat = ~Lat_DD,
                       mode = 'scattermapbox',
                       color = ~ObsType,
                       colors= ObsTypeColor,
                       alpha = 1,
                       source = "map",
                       customdata = ~ID,
                       text = ~paste0(
                         '<b>DateTime</b>: ', paste(hour(DateTime),":",minute(DateTime),":",second(DateTime)), '<br>',
                         '<b>Speed (Knt)</b>: ', Speed_N, '<br>',
                         '<b>Course (TN)</b>: ', Course_TN, '<br>'
                         )#,
                       # hovertemplate = paste(
                       #   "Time: %{text|%H:%M:%S}"
                       # )
                       ) %>%
        layout(plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
               mapbox = list(style = "satellite",
                             zoom = zoom,
                             center = list(
                               lat = center[[1]],
                               lon = center[[2]]))
        ) %>%
      event_register("plotly_selected")
    })

    # DataTable used to visualize the state of the (subset of) DataSynthesis.csv file
    # output$DataTable <- DT::renderDataTable(
    #   DT::datatable(UpApla()[UpApla()$ID %in% selected()$customdata,],
    #                 extensions = c("Buttons", "Scroller", "Select"),
    #                 filter = "top",
    #                 escape = TRUE, rownames = FALSE,
    #                 style = "bootstrap",
    #                 class = "compact",
    #                 options = list(
    #                   dom = "Brtip",
    #                   select = list(style = 'os', items = 'row'),
    #                   buttons = list(I("colvis"),"selectNone","csv"),
    #                   columnDefs = list(
    #                     list(
    #                       visible = FALSE,
    #                       targets = c(0,2,3)
    #                     )),
    #                   deferRender = TRUE,
    #                   scrollY = 100,
    #                   pageLength = 10,
    #                   scroller = TRUE
    #                 ),
    #                 selection = "none",
    #                 editable = F
    #   ),
    #   server=FALSE,
    #   editable=T
    # )

    list(
      UpApla = UpApla,
      Selected = Selected
    )

  })
}

## To be copied in the UI
# mod_selection_display_ui("selection_display_1")

## To be copied in the server
# mod_selection_display_server("selection_display_1")
