#' L2_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L2_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    Map = plotlyOutput(ns("Map"), width = NULL, height = 250)#,
    #Obs1 = mod_L1bL2_ui(ns("L1bL2_1")) #uiOutput(ns("Obs1"))
  )
}

#' L2_select Server Functions
#'
#' @noRd
mod_L2_select_server <- function(id, DB, ManObs, Obs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    SelUUID <- reactiveVal()

    observeEvent(
      event_data("plotly_click", source = "map"),
      label = "Click Obs display DB",
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
        }
      }
    )

    # observeEvent(
    #   ignoreInit = T,
    #   i() != 0,
    #   {
    #
    #     browser()
    #
    #
    #
    #   })

    # output$testX <- renderText("In Your Face !")


    i <- reactiveVal(0)

    observeEvent(
      ignoreInit = T,
      Obs$Metadata,
      {
        browser()

        i(isolate(i() + 1))

        if (i() > 4) {
          i(1)
          ObsList[[paste0("Obs",i())]] <- Obs
        } else {
          ObsList[[paste0("Obs",i())]] <- Obs
        }

        mod_L1bL2_server(paste0("L1bL2_", i()), ObsList[[paste0("Obs",i())]])

        insertUI(
          selector = paste0("#Obs", i()),
          where = "afterBegin",
          ui = tagList(
            mod_L1bL2_ui(ns(paste0("L1bL2_", i())))
          )
        )

      }
    )

    ObsList <- reactiveValues()

    #mod_L1bL2_server("L1bL2_1", ObsList$Obs1)

    # output$Obs1 <- renderUI({
    #   mod_L1bL2_ui(ns("L1bL2_1"))
    # })

    output$Obs2 <- renderUI({

    })

    output$Obs3 <- renderUI({

    })

    output$Obs4 <- renderUI({

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
          source = "map"
        ) %>% PlotDef()

        # To get the map objects reference
        #htmltools::save_html(plotly_json(p), file.path(app_sys("doc"), "map_json.hmtl"))

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
          source = "map",
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
