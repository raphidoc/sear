#' selection_display UI Function
#'
#' @description a shiny Module.
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
        uiOutput(ns("FilterInstdate")),
        uiOutput(ns("FiltertimeSpeed"))
      ),
      column(
        width = 6,
        plotlyOutput(ns("boat_raa"), width = NULL, height = 100),
        textOutput(ns("SurveyDuration")) # ,
        # selectInput(ns("Style"), "Select a mapbox style", MapStyles)
      )
    ),
    plotlyOutput(ns("Map"), width = NULL, height = 500) # ,
    # DT::DTOutput(ns("DataTable"), width = NULL, height = 100)
  )
}

# get all the available mapbox styles
# MapStyles <- schema()$layout$layoutAttributes$mapbox$style$values

#' selection_display Server Functions
#'
#' @noRd
mod_L1a_select_server <- function(id, MainLog, DB, Obs, ManObs, L1a) {
  stopifnot(is.reactive(MainLog))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filters for data selection ----------------------------------------------
    output$FilterInstdate <- renderUI({
      req(MainLog())

      tagList(
        checkboxGroupInput(
          ns("instrument"), "Intrument",
          choices = L1a$instrumentList(),
          selected = L1a$instrumentList(),
          inline = TRUE,
          width = NULL,
          choiceNames = NULL,
          choiceValues = NULL
        ),
        dateRangeInput(
          ns("dateFilter"), "date",
          start = min(lubridate::date(MainLog()$date_time)),
          end = max(lubridate::date(MainLog()$date_time)),
          min = min(lubridate::date(MainLog()$date_time)),
          max = max(lubridate::date(MainLog()$date_time))
        )
      )
    })

    output$FiltertimeSpeed <- renderUI({
      req(Allowedtime())

      tagList(
        sliderInput(
          ns("timeFilter"), "time",
          min = min(Allowedtime()),
          max = max(Allowedtime()),
          value = c(min(Allowedtime()), max = max(Allowedtime())),
          timeFormat = "%T",
          timezone = "+0000",
          width = NULL,
          step = 1
        ),
        sliderInput(ns("sol_aziLimit"), "boat_raa [degree]", value = c(45, 180), min = 0, max = 360),
        numericInput(ns("SpeedLimit"), "speed [km/h]", 11, step = 0.1)
      )
    })

    dateInterval <- reactive({
      req(input$dateFilter)
      lubridate::interval(input$dateFilter[1], input$dateFilter[2])
    })

    Allowedtime <- reactive({
      req(dateInterval())

      Int <- dateInterval()
      IntStart <- int_start(dateInterval())
      IntEnd <- int_end(dateInterval())

      # Make the IntEnd day inclusive for %within%
      int_end(Int) <- IntEnd + days(1) - seconds(1)

      if (IntStart == IntEnd) {
        MainLog()$date_time[lubridate::date(MainLog()$date_time) == IntStart]
      } else {
        MainLog()$date_time[MainLog()$date_time %within% Int]
      }
    })

    timeInterval <- reactive({
      lubridate::interval(input$timeFilter[1], input$timeFilter[2])
    })

    SubMainLog <- reactiveVal({})

    observe({
      req(timeInterval(), input$SpeedLimit, input$sol_aziLimit, input$instrument)

      Inst <- input$instrument

      PrimMainLog <- MainLog()

      if (any(str_detect(Inst, "HOCR"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$HOCR == T, ]
      }

      if (any(str_detect(Inst, "SBE19"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$SBE19 == T, ]
      }

      if (any(str_detect(Inst, "SeaOWL"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$SeaOWL == T, ]
      }

      if (any(str_detect(Inst, "BBFL2"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$BBFL2 == T, ]
      }

      if (any(str_detect(Inst, "BioSonic"))) {
        PrimMainLog <- PrimMainLog[PrimMainLog$BioSonic == T, ]
      }

      PrimMainLog <- PrimMainLog %>%
        filter(
          date_time %within% timeInterval(),
          boat_raa > input$sol_aziLimit[1] & boat_raa < input$sol_aziLimit[2],
          speed_kmh <= input$SpeedLimit
        )

      SubMainLog(PrimMainLog)
    })

    # Selected data points ----------------------------------------------------
    # Get the id of plotly_mapbox selected point in: selected()$customdata
    # Selected <- reactiveVal({})

    SelID <- eventReactive(
      event_data("plotly_selected", source = "map"),
      label = "Select data",
      ignoreInit = F,
      {
        validate(need(!identical(event_data("plotly_selected", source = "map"), list()), message = "No data selected"))

        # curvenumber 0 is the Applanix trace
        # warning("To Raph, curvenumber must equal 0 or 1")
        id <- event_data("plotly_selected", source = "map") %>%
          filter(curveNumber == 1 | curveNumber == 0)

        # When Obs trace is selected, uuid_l2 make custom data as character
        id <- as.numeric(id$customdata)

        # Check that all id form a continuous sequence with increment of one
        if (!all(abs(diff(id)) == 1)) {
          showModal(modalDialog(
            title = "Invalid selection",
            "You selected discontinous data, please select only contiguous points",
            easyClose = TRUE
          ))
          # invalidatelater(1)
          id
        } else {
          id
        }
      }
    )

    # Define zoom and center reactive value to be updated with Seluuid_l2

    Center <- reactiveVal()
    Zoom <- reactiveVal()

    Seluuid_l2 <- reactiveVal()

    observeEvent(
      event_data("plotly_click", source = "map"),
      label = "Click Obs select data",
      ignoreInit = T,
      {
        uuid_l2 <- as.character(event_data("plotly_click", source = "map")$customdata)

        if (!identical(uuid_l2, character(0)) && !uuid::UUIDvalidate(uuid_l2)) {
          showModal(modalDialog(
            title = "Invalid click",
            "You didn't click on an Obs feature, no uuid_l2 attatched",
            easyClose = TRUE
          ))
          invalidatelater(1)
        } else {
          Seluuid_l2(uuid_l2)
          Center(DB$ObsMeta() %>% filter(uuid_l2 == Seluuid_l2()) %>% select(lat, lon))
          Zoom(20)
        }
      }
    )

    observeEvent(
      DB$ObsSel(),
      label = "Select Obs",
      {
        uuid_l2 <- DB$ObsSel()

        if (!uuid::UUIDvalidate(uuid_l2)) {
          invalidatelater(1)
        } else {
          Seluuid_l2(uuid_l2)
          Center(DB$ObsMeta() %>% filter(uuid_l2 == Seluuid_l2()) %>% select(lat, lon))
          Zoom(20)
        }
      }
    )

    observeEvent(
      ManObs$Save(),
      {
        uuid_l2 <- Obs$metadata_l2$uuid_l2

        Seluuid_l2(uuid_l2)
        Center(Obs$metadata_l2 %>% select(lat, lon))
        Zoom(20)
      }
    )

    SelMainLog <- reactive({
      req(SelID())

      # Should not be recomputed when processing to L1b ...

      MainLog()[MainLog()$id %in% SelID(), ]
    })

    # Map for data selection --------------------------------------------------

    # TODO: GitHub issue #11

    # observeEvent(
    #   Seluuid_l2(),
    #   {
    #     plotlyProxy("Map", session) %>%
    #       plotlyProxyInvoke(
    #         "addTraces",
    #         list(
    #           lon = list(Center()[[2]]),
    #           lat = list(Center()[[1]]),
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

      if (is.null(Seluuid_l2())) {
        ZC <- zoom_center(SubMainLog()$lat, SubMainLog()$lon)
        Zoom(ZC[[1]])
        Center(ZC[[2]])
      }

      # SF read coords as XY not YX aka lat lon
      # ObsMeta <- sf::st_as_sf(DB$ObsMeta(), coords = c("lon", "lat"), crs = 4326) %>% sf::st_transform(2947)
      # ObsMetaBuffer <- sf::st_buffer(x = ObsMeta, dist = ObsMeta$distance_run / 2) %>% sf::st_transform(4326)

      # Avoid sfheaders::sf_to_df bug if object empty
      # if (nrow(ObsMetaBuffer) == 0) {
      #   ObsMetaBuffer <- tibble(
      #     uuid_l2 = NA,
      #     x = NA,
      #     y = NA
      #   )
      # } else {
      #   ObsMetaBuffer <- sfheaders::sf_to_df(ObsMetaBuffer, fill = T)
      # }

      # plot definition
      PlotDef <- function(.) {
        (.) %>%
          add_markers(
            name = "Raw",
            data = SubMainLog(),
            x = ~lon,
            y = ~lat,
            customdata = ~id,
            marker = list(color = "rgb(154, 42, 42)"),
            text = ~ paste0(
              "<b>date</b>: ", format(date_time, "%Y-%m-%d"), "<br>",
              "<b>time</b>: ", format(date_time, "%H:%M:%S"), "<br>",
              "<b>speed (km/h)</b>: ", speed_kmh, "<br>",
              # "<b>Course (TN)</b>: ", course_tn, "<br>",
              "<b>boat_raa (degree)</b>: ", boat_raa, "<br>"
            )
          ) %>%
          # add_polygons( # When add_sf is used a center and zoom animation is enable and I dont know how to control it
          #   name = "ObsBuffer",
          #   data = ObsMetaBuffer,
          #   x = ~x,
          #   y = ~y,
          #   customdata = ~uuid_l2,
          #   line = list(color = "rgb(127, 255, 212)", width = 1),
          #   fillcolor = "rgba(127, 255, 212, 0.2)",
          #   split = ~uuid_l2,
          #   legendgroup = "Obs",
          #   showlegend = F) %>%
          add_markers(
            name = "Obs",
            data = DB$ObsMeta(),
            x = ~lon,
            y = ~lat,
            customdata = ~uuid_l2,
            marker = list(
              color = "rgb(127, 255, 212)" # ,
              # size = ~ distance_run
            ),
            text = ~ paste0(
              "<b>date_time</b>: ", date_time, "<br>",
              "<b>uuid_l2</b>: ", uuid_l2, "<br>"
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
        # htmltools::save_html(plotly_json(p), file.path(app_sys("doc"), "map_json.hmtl"))
      } else {
        # Determine survey area bounding box and crop coastline accordingly

        SurveyArea <- sf::st_as_sf(SubMainLog(), coords = c("lon", "lat"), crs = 4326) %>%
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

      # Save graph
      # save_image(p, file=file.path(path.expand("~"), "sear_figure", "map.png"), scale = 3, height = 720, width = 1280)

      p
    })

    # boat_raa polar plot ---------------------------------------------------
    output$boat_raa <- renderPlotly({
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
          r = ~speed_kmh,
          theta = ~boat_raa,
          mode = "markers"
        ) %>%
        layout(
          autosize = F,
          margin = m,
          polar = list(
            angularaxis = list(
              # rotation = ~mean(course_tn, na.rm = T),
              direction = "clockwise"
            )
          )
        )
    })


    # Infos on displayed data -------------------------------------------------

    output$SurveyDuration <- renderText({
      validate(need(nrow(SubMainLog()) != 0, message = "No data to display with those filters"))

      time <- SubMainLog()$date_time

      as.character(dseconds(length(time)))
    })

    # Module output -----------------------------------------------------------
    list(
      # MainLog = MainLog,
      SubMainLog = SubMainLog,
      MainLog = SelMainLog,
      SelID = SelID,
      Seluuid_l2 = Seluuid_l2,
      Map = Map
    )
  })
}

## To be copied in the UI
# mod_L1a_select_ui("L1a_select")

## To be copied in the server
# mod_L1a_select_server("L1a_select")
