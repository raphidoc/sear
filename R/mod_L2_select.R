#' L2_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import d3Tree
mod_L2_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        plotlyOutput(ns("Map")),
        uiOutput(ns("InstX")),
        uiOutput(ns("VarX")),
        uiOutput(ns("InstY")),
        uiOutput(ns("VarY"))
      ),
      column(
        width = 6,
        plotlyOutput(ns("Plot"))
      )
    )



  )
}

#' L2_select Server Functions
#'
#' @noRd
mod_L2_select_server <- function(id, DB, ManObs, L2Obs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    SelUUID <- reactiveVal()

    observeEvent(
      event_data("plotly_selected", source = "L2map"),
      label = "Click Obs display DB",
      ignoreInit = T,
      {

        UUID <- as.character(event_data("plotly_selected", source = "L2map")$customdata)

        if (!identical(UUID, character(0)) && any(!uuid::UUIDvalidate(UUID))) {
          showModal(modalDialog(
            title = "Invalid selection",
            "You didn't select an L2Obs feature, no UUID attatched"
          ))
          invalidateLater(1)
        } else {
          SelUUID(UUID)
        }

      }
    )

    InstList <- reactiveVal()

    observeEvent(
      nrow(L2Obs$Metadata != 0),
      {
        Instruments <- str_subset(names(L2Obs), "[^(Metadata)]")

        InstList(c("",Instruments))

      }
    )

    output$InstX <- renderUI({
      req(InstList())
      selectizeInput(
        ns("InstX"),
        "Select a x instrument",
        choices = InstList(),
        selected = NULL,
        multiple = F)
    })

    output$InstY <- renderUI({
      req(InstList())
      selectizeInput(
        ns("InstY"),
        "Select a y instrument",
        choices = InstList(),
        selected = NULL,
        multiple = F)
    })

    VarListX <- reactiveVal()
    VarListY <- reactiveVal()

    observeEvent(
      req(input$InstX != ""),
      {
        Variables <- str_subset(names(L2Obs[[input$InstX]]), "[^(UUID)(Wavelength)]")

        VarListX(c("",Variables))
      }
    )

    observeEvent(
      req(input$InstY != ""),
      {
        Variables <- str_subset(names(L2Obs[[input$InstY]]), "[^(UUID)(Wavelength)]")

        VarListY(c("",Variables))
      }
    )

    output$VarX <- renderUI({
      req(VarListX())
      selectizeInput(
        ns("VarX"),
        "Select a x variable",
        choices = VarListX(),
        selected = NULL,
        multiple = F)
    })

    output$VarY <- renderUI({
      req(VarListY())
      selectizeInput(
        ns("VarY"),
        "Select a y variable",
        choices = VarListY(),
        selected = NULL,
        multiple = F)
    })

    # VarX <- eventReactive(
    #   ignoreNULL = T,
    #   req(network$click$value == "Variables"),
    #   {
    #
    #     L2Obs[["HOCR"]][[VarX]]
    #
    #
    #   }
    # )

    # TODO: use plotly proxy to link (highlight) map and plot
    # by adding a trace or modifying one

    # observeEvent(
    #   {
    #     event_data("plotly_hover", source = "plot")
    #   },{
    #
    #     HovUUID <- event_data("plotly_hover", source = "plot")$customdata
    #
    #     HovMark <- L2Obs$Metadata %>% filter(UUID == HovUUID)
    #
    #
    #     plotlyProxy("Map", session) %>%
    #       plotlyProxyInvoke(
    #         "addTraces",
    #         list(
    #           lon = list(HovMark$Lon),
    #           lat = list(HovMark$Lat),
    #           type = list("scattermapbox"),
    #           mode = list("markers"),
    #           marker.color = list("#FF0000")
    #         ),
    #         list(0)
    #       )
    #
    #
    #   }
    # )

    output$Plot <- renderPlotly({
      req(nrow(L2Obs$Metadata != 0))
      req(input$VarY)
      validate(need(input$VarY != "", message = "Need x and y variables"))

      InstX <- input$InstX
      InstY <- input$InstY
      VarX <- input$VarX
      VarY <- input$VarY

      L2 <- L2Obs$Metadata

      for (i in names(L2Obs)[-1]) {
        L2 <- left_join(L2, L2Obs[[i]], by = c("UUID"))
      }

      if (InstX != "" && InstY != "") {

        p <- L2 %>%
          filter(Wavelength %in% c(401,500,602,701)) %>%
          plot_ly(
            source = "plot",
            text = ~UUID,
            customdata = ~UUID
          ) %>%
          add_markers(
            x = ~.data[[VarX]],
            y = ~.data[[VarY]],
            color = ~as.character(Wavelength),
            showlegend = T
            )%>%
          event_register("plotly_hover")

      }

      # In case of spectral data
      if (InstX == "" && any(VarY %in% c("Rrs","KLu"))) {

        p <- L2 %>%
          plot_ly(
            source = "plot",
            text = ~UUID,
            customdata = ~UUID
          ) %>%
          add_lines(x = ~.data[["Wavelength"]], y = ~.data[[VarY]], showlegend = F)%>%
          event_register("plotly_hover")

      }

      p

    })


    output$Map <- renderPlotly({
      req(DB$ObsMeta())

      validate(need(nrow(DB$ObsMeta()) != 0, message = "Empty DB"))

      Center <- reactiveVal()
      Zoom <- reactiveVal()

      ZC <- zoom_center(DB$ObsMeta()$Lat, DB$ObsMeta()$Lon)
      Zoom(ZC[[1]])
      Center(ZC[[2]])

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
          event_register("plotly_click")
      }

      if (curl::has_internet() # & curl::curl_fetch_memory("https://www.mapbox.com/")$status_code == 200
      ) {
        p <- plot_mapbox(
          mode = "scattermapbox",
          source = "L2map",
          unselected = list(
            marker = list(
              opacity = 0.4
            )
          )
        ) %>% PlotDef()

        # To get the map objects reference
        #htmltools::save_html(plotly_json(p), file.path(app_sys("doc"), "map_json.html"))

      } else {
        # Determine survey area bounding box and crop coastline accordingly

        SurveyArea <- sf::st_as_sf(DB$ObsMeta(), coords = c("Lon", "Lat"), crs = 4326) %>%
          select(geometry) %>%
          summarise()

        SurveyArea <- sf::st_cast(x = SurveyArea, to = "MULTILINESTRING")

        SurveyArea <- sf::st_buffer(x = SurveyArea, dist = 5000) # %>% sf::st_transform(4326)

        BBox <- sf::st_bbox(SurveyArea)

        Coast <- sf::st_read(app_sys("intdata/ne_10m_coastline/ne_10m_coastline.shp")) %>% select(geometry)

        CoastCrop <- sf::st_crop(Coast, BBox)

        p <- plot_ly(
          source = "L2map",
          unselected = list(
            marker = list(
              opacity = 0.4
            )
          )
        ) %>%
          add_sf(data = CoastCrop) %>%
          PlotDef(.)
      }

      p

    })


    # Module output -----------------------------------------------------------

    list(
      SelUUID = SelUUID
    )

  })
}

## To be copied in the UI
# mod_L2_select_ui("L2_select")

## To be copied in the server
# mod_L2_select_server("L2_select")
