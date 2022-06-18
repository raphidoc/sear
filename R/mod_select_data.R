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

mod_select_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             uiOutput(ns("Filters"))
             ),
      column(width = 6,
             plotlyOutput(ns("BoatSolAzm"), width = NULL, height = 100)
             )
    ),
    plotlyOutput(ns("Map"), width = NULL, height = 500)#,
    #DT::DTOutput(ns("DataTable"), width = NULL, height = 100)

  )
}

#' selection_display Server Functions
#'
#' @noRd
mod_select_data_server <- function(id, Apla, DB){

  stopifnot(is.reactive(Apla))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Update tibble ObsType and ObsName on ApplyObs button click event
    UpApla <- reactiveVal({})

    observe({
      UpApla(Apla())
    })

# Filters for data selection ----------------------------------------------
    output$Filters <- renderUI({

      req(Apla())

      tagList(
        sliderInput(ns("TimeFilter"), "Time",
                    min = min(Apla()$DateTime), max = max(Apla()$DateTime),
                    value = c(min(Apla()$DateTime), max = max(Apla()$DateTime)),
                    timeFormat = "%T",
                    timezone = "+0000",
                    width = NULL,
                    step = 1),
        sliderInput(ns("SolAzmLimit"), "BoatSolAzm", value = c(0, 180), min = 0, max = 360),
        numericInput(ns("SpeedLimit"), "Speed", 4, step = 0.1)
      )
    })

    TimeInterval <- reactive({
      lubridate::interval(input$TimeFilter[1],input$TimeFilter[2])
    })

    SubUpApla <- reactiveVal({})

    observe({
      req(TimeInterval(), input$SpeedLimit)

      AplaTime <- UpApla() %>%
        filter(DateTime %within% TimeInterval(),
               BoatSolAzm > input$SolAzmLimit[1] & BoatSolAzm < input$SolAzmLimit[2],
               Speed_N <= input$SpeedLimit)

      SubUpApla(AplaTime)
    })

# Selected data points ----------------------------------------------------
    # Get the ID of plotly_mapbox selected point in: selected()$customdata
    #Selected <- reactiveVal({})

    SelID <- eventReactive(
      event_data("plotly_selected", source = "map"),
      label = "Select data",
      ignoreInit = T,
      {

        # curvenumber 0 is the Applanix trace
        ID <- event_data("plotly_selected", source = "map") %>%
          filter(curveNumber == 0)

        # When Obs trace is selected, UUID make custom data as character
        ID <- as.numeric(ID$customdata)

        # Check that all ID form a continuous sequence with increment of one
        if (!all(abs(diff(ID)) == 1)) {
          showModal(modalDialog(
            title = "Invalid selection",
            "You selected discontinous data, please select only contiguous points")
          )
          invalidateLater(1)

        } else {
          ID
        }
      })

    SelUUID <- eventReactive(
      event_data("plotly_click", source = "map"),
      label = "Select Obs",
      ignoreInit = T,
      {

        UUID <- as.character(event_data("plotly_click", source = "map")$customdata)

        if (!uuid::UUIDvalidate(UUID)) {
          showModal(modalDialog(
            title = "Invalid click",
            "You didn't click on an Obs feature (Station/Transect), no UUID attatched")
          )
          invalidateLater(1)

        } else {
          UUID
        }

      }
    )

    observe(SelUUID())

    SelApla <- reactive({
      req(SelID())

      # Should not be recomputed when processing to L1b ...

      UpApla()[UpApla()$ID %in% SelID(), ]
    })


# Map for data selection --------------------------------------------------

    output$Map <- renderPlotly({
      req(SubUpApla())


      #ObsTypeColor <- c("Unknown" = "red", "Transit" = "black", "Transect" = "orange", "Station" = "green")

      zc <- zoom_center(SubUpApla()$Lat_DD, SubUpApla()$Lon_DD)
      zoom <- zc[[1]]
      center <- zc[[2]]

      p <- plot_mapbox(
        mode = 'scattermapbox',
        source = "map"
        ) %>%
        add_markers(
          name = "Raw",
          data = SubUpApla(),
          x = ~Lon_DD,
          y = ~Lat_DD,
          customdata = ~ID,
          marker = list(color = 'rgb(154, 42, 42)'),
          text = ~paste0(
            '<b>DateTime</b>: ', paste(hour(DateTime),":",minute(DateTime),":",second(DateTime)), '<br>',
            '<b>Speed (Knt)</b>: ', Speed_N, '<br>',
            '<b>Course (TN)</b>: ', Course_TN, '<br>',
            '<b>SolAzm (degree)</b>: ', SolAzm, '<br>'
          )
        ) %>%
        add_markers(
          name = "Stations",
          data = DB$ObsMeta() %>%
            filter(ObsType == "Station"),
          x = ~Lon,
          y = ~Lat,
          customdata = ~UUID,
          marker = list(color = 'rgb(127, 255, 212)'),
          text = ~paste0(
            #'<b>DateTime</b>: ', paste(hour(DateTime),":",minute(DateTime),":",second(DateTime)), '<br>',
            '<b>ObsName</b>: ', ObsName, '<br>',
            '<b>ObsType</b>: ', ObsType, '<br>'
            #'<b>Speed (Knt)</b>: ', Speed_N, '<br>',
            #'<b>Course (TN)</b>: ', Course_TN, '<br>',
            #'<b>SolAzm (degree)</b>: ', SolAzm, '<br>'
          )
        ) %>%
        layout(
          plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
          mapbox = list(style = "satellite",
                        zoom = zoom,
                        center = list(
                          lat = center[[1]],
                          lon = center[[2]]
                        )
          )
        ) %>%
        event_register("plotly_click") %>%
        event_register("plotly_selected")

      # for transect rgb(228, 208, 10)

    })

# BoatSolAzm polar plot ---------------------------------------------------
    output$BoatSolAzm <- renderPlotly({

      req(SelApla())

      m <- list(
        l = 40,
        r = 30,
        b = 30,
        t = 30,
        pad = 0
      )

      SelApla() %>%
        plot_ly(
          width = 250,
          height = 250,
          type = 'scatterpolar',
          r = ~Speed_N,
          theta = ~BoatSolAzm,
          mode = 'markers'
        ) %>%
        layout(
          autosize = F,
          margin = m,
          polar = list(
            angularaxis  = list(
              #rotation = ~mean(Course_TN, na.rm = T),
              direction = "clockwise"
            )
          ))

    })

# Module output -----------------------------------------------------------
    list(
      UpApla = UpApla,
      SelApla = SelApla,
      SelID = SelID,
      SelUUID = SelUUID,
      Map = Map
    )

  })
}

## To be copied in the UI
# mod_selection_display_ui("selection_display_1")

## To be copied in the server
# mod_selection_display_server("selection_display_1")

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
