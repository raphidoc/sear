#' selection_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1a_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        uiOutput(ns("FilterInstDate")),
        uiOutput(ns("FilterTimeSpeed"))
      ),
      column(
        width = 6,
        plotlyOutput(ns("BoatSolAzm"), width = NULL, height = 100)#,
        #selectInput(ns("Style"), "Select a mapbox style", MapStyles)
      )
    ),
    plotlyOutput(ns("Map"), width = NULL, height = 500) # ,
    # DT::DTOutput(ns("DataTable"), width = NULL, height = 100)
  )
}

# get all the available mapbox styles
#MapStyles <- schema()$layout$layoutAttributes$mapbox$style$values

#' selection_display Server Functions
#'
#' @noRd
mod_L1a_select_server <- function(id, MainLog, DB, Obs, ManObs, L1a) {
  stopifnot(is.reactive(MainLog))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filters for data selection ----------------------------------------------
    output$FilterInstDate <- renderUI({
      req(MainLog())

      tagList(
        checkboxGroupInput(
          ns("Instrument"), "Intrument",
          choices = L1a$InstrumentList(),
          selected = L1a$InstrumentList(),
          inline = TRUE,
          width = NULL,
          choiceNames = NULL,
          choiceValues = NULL
        ),
        dateRangeInput(
          ns("DateFilter"), "DateFilter",
          start = min(lubridate::date(MainLog()$DateTime)),
          end = max(lubridate::date(MainLog()$DateTime)),
          min = min(lubridate::date(MainLog()$DateTime)),
          max = max(lubridate::date(MainLog()$DateTime))
        )
      )
    })

    output$FilterTimeSpeed <- renderUI({
      req(AllowedTime())

      tagList(
        sliderInput(
          ns("TimeFilter"), "Time",
          min = min(AllowedTime()),
          max = max(AllowedTime()),
          value = c(min(AllowedTime()), max = max(AllowedTime())),
          timeFormat = "%T",
          timezone = "+0000",
          width = NULL,
          step = 1
        ),
        sliderInput(ns("SolAzmLimit"), "BoatSolAzm", value = c(90, 180), min = 0, max = 360),
        numericInput(ns("SpeedLimit"), "Speed", 6, step = 0.1)
      )
    })

    DateInterval <- reactive({
      req(input$DateFilter)
      lubridate::interval(input$DateFilter[1], input$DateFilter[2])
    })

    AllowedTime <- reactive({
      req(DateInterval())

      Int <- DateInterval()
      IntStart <- int_start(DateInterval())
      IntEnd <- int_end(DateInterval())

      # Make the IntEnd day inclusive for %within%
      int_end(Int) <- IntEnd + days(1) - seconds(1)

      if (IntStart == IntEnd) {
        MainLog()$DateTime[lubridate::date(MainLog()$DateTime) == IntStart]
      } else {
        MainLog()$DateTime[MainLog()$DateTime %within% Int]
      }

    })

    TimeInterval <- reactive({
      lubridate::interval(input$TimeFilter[1], input$TimeFilter[2])
    })

    SubMainLog <- reactiveVal({})

    observe({
      req(TimeInterval(), input$SpeedLimit, input$SolAzmLimit, input$Instrument)

      Inst <- input$Instrument

      PrimMainLog <- MainLog()

      if (any(str_detect(Inst, "HOCR"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$HOCR == T,]
      }

      if (any(str_detect(Inst, "SBE19"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$SBE19 == T,]
      }

      if (any(str_detect(Inst, "SeaOWL"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$SeaOWL == T,]
      }

      if (any(str_detect(Inst, "BBFL2"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$BBFL2 == T,]
      }

      if (any(str_detect(Inst, "BioSonic"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$BioSonic == T,]
      }

      PrimMainLog <- PrimMainLog %>%
        filter(
          DateTime %within% TimeInterval(),
          BoatSolAzm > input$SolAzmLimit[1] & BoatSolAzm < input$SolAzmLimit[2],
          Speed_N <= input$SpeedLimit
        )

      SubMainLog(PrimMainLog)
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
      label = "Click Obs select data",
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

    SelMainLog <- reactive({
      req(SelID())

      # Should not be recomputed when processing to L1b ...

      MainLog()[MainLog()$ID %in% SelID(), ]
    })


    # Map for data selection --------------------------------------------------

    # TODO: GitHub issue #11

    # observeEvent(
    #   SelUUID(),
    #   {
    #     plotlyProxy("Map", session) %>%
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

    ### User input mapbox style
    # observeEvent(
    #   input$Style,
    #   {
    #   plotlyProxy("Map", session) %>%
    #     plotlyProxyInvoke(
    #       "relayout",
    #       list(mapbox = list(style = input$Style))
    #     )
    # })


    output$Map <- renderPlotly({
      req(SubMainLog())

      validate(need(nrow(SubMainLog()) != 0, message = "No data to display with those filters"))

      if (is.null(SelUUID())) {
        ZC <- zoom_center(SubMainLog()$Lat, SubMainLog()$Lon)
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
            data = SubMainLog(),
            x = ~Lon,
            y = ~Lat,
            customdata = ~ID,
            marker = list(color = "rgb(154, 42, 42)"),
            text = ~ paste0(
              "<b>Date</b>: ", format(DateTime, "%Y-%m-%d"), "<br>",
              "<b>Time</b>: ", format(DateTime, "%H:%M:%S"), "<br>",
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
          source = "map",
          selected = list(
            marker = list(
              color = "green"
            )
          ),
          unselected = list(
            marker = list(
              opacity = 0.7
            )
          )
        ) %>% PlotDef()

        # To get the map objects reference
        #htmltools::save_html(plotly_json(p), file.path(app_sys("doc"), "map_json.hmtl"))

      } else {
        # Determine survey area bounding box and crop coastline accordingly

        SurveyArea <- sf::st_as_sf(SubMainLog(), coords = c("Lon", "Lat"), crs = 4326) %>%
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
      req(SelMainLog())

      m <- list(
        l = 40,
        r = 30,
        b = 30,
        t = 30,
        pad = 0
      )

      SelMainLog() %>%
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
      #MainLog = MainLog,
      SubMainLog = SubMainLog,
      MainLog = SelMainLog,
      SelID = SelID,
      SelUUID = SelUUID,
      Map = Map
    )
  })
}

## To be copied in the UI
# mod_L1a_select_ui("L1a_select")

## To be copied in the server
# mod_L1a_select_server("L1a_select")
