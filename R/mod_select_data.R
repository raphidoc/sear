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
mod_select_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        uiOutput(ns("Filters"))
      ),
      column(
        width = 6,
        plotlyOutput(ns("BoatSolAzm"), width = NULL, height = 100)
      )
    ),
    plotlyOutput(ns("Map"), width = NULL, height = 500) # ,
    # DT::DTOutput(ns("DataTable"), width = NULL, height = 100)
  )
}

#' selection_display Server Functions
#'
#' @noRd
mod_select_data_server <- function(id, Apla, DB, Obs, ManObs) {
  stopifnot(is.reactive(Apla))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filters for data selection ----------------------------------------------
    output$Filters <- renderUI({
      req(Apla())

      tagList(
        sliderInput(ns("TimeFilter"), "Time",
          min = min(Apla()$DateTime), max = max(Apla()$DateTime),
          value = c(min(Apla()$DateTime), max = max(Apla()$DateTime)),
          timeFormat = "%F %T",
          timezone = "+0000",
          width = NULL,
          step = 1
        ),
        sliderInput(ns("SolAzmLimit"), "BoatSolAzm", value = c(90, 180), min = 0, max = 360),
        numericInput(ns("SpeedLimit"), "Speed", 6, step = 0.1)
      )
    })

    DateTimeInterval <- reactive({
      lubridate::interval(input$TimeFilter[1], input$TimeFilter[2])
    })

    SubApla <- reactiveVal({})

    observe({
      req(DateTimeInterval(), input$SpeedLimit, input$SolAzmLimit)

      AplaTime <- Apla() %>%
        filter(
          DateTime %within% DateTimeInterval(),
          BoatSolAzm > input$SolAzmLimit[1] & BoatSolAzm < input$SolAzmLimit[2],
          Speed_N <= input$SpeedLimit
        )

      SubApla(AplaTime)
    })

    # Selected data points ----------------------------------------------------
    # Get the ID of plotly_mapbox selected point in: selected()$customdata
    # Selected <- reactiveVal({})

    SelID <- eventReactive(
      event_data("plotly_selected", source = "map"),
      label = "Select data",
      ignoreInit = F,
      {

        if (is.list(event_data("plotly_selected", source = "map"))) {
          invalidateLater(1)
        }

        # curvenumber 0 is the Applanix trace
        ID <- event_data("plotly_selected", source = "map") %>%
          filter(curveNumber == 0)

        # When Obs trace is selected, UUID make custom data as character
        ID <- as.numeric(ID$customdata)

        # Check that all ID form a continuous sequence with increment of one
        if (!all(abs(diff(ID)) == 1)) {
          showModal(modalDialog(
            title = "Invalid selection",
            "You selected discontinous data, please select only contiguous points"
          ))
          # invalidateLater(1)
          ID
        } else {
          ID
        }
      }
    )

    # Define zoom and center reactive value to be updated with SelUUID

    Center <- reactiveVal()
    Zoom <- reactiveVal()

    SelUUID <- reactiveVal()

    observeEvent(
      event_data("plotly_click", source = "map"),
      label = "Click Obs",
      ignoreInit = T,
      {
        UUID <- as.character(event_data("plotly_click", source = "map")$customdata)

        if (!identical(UUID, character(0)) && !uuid::UUIDvalidate(UUID)) {
          showModal(modalDialog(
            title = "Invalid click",
            "You didn't click on an Obs feature, no UUID attatched"
          ))
          invalidateLater(1)
        } else {
          SelUUID(UUID)
          Center(DB$ObsMeta() %>% filter(UUID == SelUUID()) %>% select(Lat, Lon))
          Zoom(20)
        }
      }
    )

    observeEvent(
      DB$ObsSel(),
      label = "Select Obs",
      {
        UUID <- DB$ObsSel()

        if (!uuid::UUIDvalidate(UUID)) {
          invalidateLater(1)
        } else {
          SelUUID(UUID)
          Center(DB$ObsMeta() %>% filter(UUID == SelUUID()) %>% select(Lat, Lon))
          Zoom(20)
        }
      }
    )

    observeEvent(
      ManObs$Save(),
      {
        UUID <- Obs$Metadata$UUID

        SelUUID(UUID)
        Center(Obs$Metadata %>% select(Lat, Lon))
        Zoom(20)
      }
    )

    SelApla <- reactive({
      req(SelID())

      # Should not be recomputed when processing to L1b ...

      Apla()[Apla()$ID %in% SelID(), ]
    })


    # Map for data selection --------------------------------------------------

    # TODO: GitHub issue #11

    # observeEvent(
    #   SelUUID(),
    #   {
    #     plotlyProxy("map", session) %>%
    #       plotlyProxyInvoke(
    #         "addTraces",
    #         list(
    #           Lon = list(Center()[[2]]),
    #           Lat = list(Center()[[1]]),
    #           marker.color = list('rgb(255, 0, 0)')
    #         )
    #       )
    #   }
    # )

    output$Map <- renderPlotly({
      req(SubApla())

      if (is.null(SelUUID())) {
        ZC <- zoom_center(SubApla()$Lat_DD, SubApla()$Lon_DD)
        Zoom(ZC[[1]])
        Center(ZC[[2]])
      }

      # SF read coords as XY not YX aka Lat Lon
      ObsMeta <- sf::st_as_sf(DB$ObsMeta(), coords = c("Lon", "Lat"), crs = 4326) %>% sf::st_transform(2947)
      ObsMetaBuffer <- sf::st_buffer(x = ObsMeta, dist = ObsMeta$DistanceRun / 2) %>% sf::st_transform(4326)

      # Avoid sfheaders::sf_to_df bug if object empty
      if (nrow(ObsMetaBuffer) == 0) {
        ObsMetaBuffer <- tibble(
          UUID = NA,
          x = NA,
          y = NA
        )
      } else {
        ObsMetaBuffer <- sfheaders::sf_to_df(ObsMetaBuffer, fill = T)
      }

      # plot definition
      PlotDef <- function(.) {
        (.) %>%
          add_markers(
            name = "Raw",
            data = SubApla(),
            x = ~Lon_DD,
            y = ~Lat_DD,
            customdata = ~ID,
            marker = list(color = "rgb(154, 42, 42)"),
            text = ~ paste0(
              "<b>DateTime</b>: ", paste(hour(DateTime), ":", minute(DateTime), ":", second(DateTime)), "<br>",
              "<b>Speed (Knt)</b>: ", Speed_N, "<br>",
              "<b>Course (TN)</b>: ", Course_TN, "<br>",
              "<b>BoatSolAzm (degree)</b>: ", BoatSolAzm, "<br>"
            )
          ) %>%
          add_polygons( # When add_sf is used a center and zoom animation is enable and I dont know how to control it
            name = "ObsBuffer",
            data = ObsMetaBuffer,
            x = ~x,
            y = ~y,
            customdata = ~UUID,
            line = list(color = "rgb(127, 255, 212)", width = 1),
            fillcolor = "rgba(127, 255, 212, 0.2)",
            split = ~UUID,
            legendgroup = "Obs",
            showlegend = F
          ) %>%
          add_markers(
            name = "Obs",
            data = DB$ObsMeta(),
            x = ~Lon,
            y = ~Lat,
            customdata = ~UUID,
            marker = list(color = "rgb(127, 255, 212)"),
            text = ~ paste0(
              "<b>ObsName</b>: ", ObsName, "<br>",
              "<b>DateTime</b>: ", DateTime, "<br>",
              "<b>UUID</b>: ", UUID, "<br>"
            ),
            legendgroup = "Obs"
          ) %>%
          layout(
            plot_bgcolor = "#191A1A", paper_bgcolor = "#191A1A",
            mapbox = list(
              style = "satellite",
              zoom = Zoom(),
              center = list(
                lat = Center()[[1]],
                lon = Center()[[2]]
              )
            )
          ) %>%
          event_register("plotly_click") %>%
          event_register("plotly_selected")
      }

      if (curl::has_internet() # & curl::curl_fetch_memory("https://www.mapbox.com/")$status_code == 200
      ) {
        p <- plot_mapbox(
          mode = "scattermapbox",
          source = "map"
        ) %>% PlotDef()

        htmltools::save_html(plotly_json(p), file.path(app_sys("doc"), "map_json.hmtl"))
      } else {
        # Determine survey area bounding box and crop coastline accordingly

        SurveyArea <- sf::st_as_sf(SubApla(), coords = c("Lon_DD", "Lat_DD"), crs = 4326) %>%
          select(geometry) %>%
          summarise()

        SurveyArea <- sf::st_cast(x = SurveyArea, to = "MULTILINESTRING")

        SurveyArea <- sf::st_buffer(x = SurveyArea, dist = 3000) # %>% sf::st_transform(4326)

        BBox <- sf::st_bbox(SurveyArea)

        Coast <- sf::st_read(app_sys("intdata/ne_10m_coastline/ne_10m_coastline.shp")) %>% select(geometry)

        CoastCrop <- sf::st_crop(Coast, BBox)

        p <- plot_ly(
          source = "map",
        ) %>%
          add_sf(data = CoastCrop) %>%
          PlotDef(.)
      }

      p
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
          type = "scatterpolar",
          r = ~Speed_N,
          theta = ~BoatSolAzm,
          mode = "markers"
        ) %>%
        layout(
          autosize = F,
          margin = m,
          polar = list(
            angularaxis = list(
              # rotation = ~mean(Course_TN, na.rm = T),
              direction = "clockwise"
            )
          )
        )
    })

    # Module output -----------------------------------------------------------
    list(
      Apla = Apla,
      SubApla = SubApla,
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
