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
    Map = box(plotlyOutput(ns("Map"), width = NULL, height = 250)),
    column(
      width = 6,
      uiOutput(ns("Hierarchy")),
      verbatimTextOutput(ns("results")),
      tableOutput(ns("clickView")),
      d3treeOutput(
        outputId = ns("d3x"),
        width = '1200px',
        height = '475px'
      )
    ),
    column(6,
           tableOutput(ns('table'))
    )


    #uiOutput(ns("Plot")),
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
      event_data("plotly_selected", source = "map"),
      label = "Click Obs display DB",
      ignoreInit = T,
      {

        UUID <- as.character(event_data("plotly_selected", source = "map")$customdata)

        if (!identical(UUID, character(0)) && any(!uuid::UUIDvalidate(UUID))) {
          showModal(modalDialog(
            title = "Invalid selection",
            "You didn't select an Obs feature, no UUID attatched"
          ))
          invalidateLater(1)
        } else {
          SelUUID(UUID)
        }
      }
    )

    VarList <- reactiveVal()

    observeEvent(
      nrow(L2Obs$Metadata != 0),
      {

        browser()

        Instruments <- str_subset(names(L2Obs), "[^(Metadata)]")

        Variables <- list()

        for (i in Instruments) {
          Variables[i] <- list(str_subset(names(L2Obs[[i]]), "[^(UUID)(Wavelength)]"))
        }

        Variables <- stack(Variables) %>%
          rename(Instruments = ind, Variables = values) %>%
          relocate(Instruments, Variables) %>%
          mutate(NEWCOL=NA) %>% data.frame

        VarList(Variables)

        # for (i in Instruments) {
        #   Variables[i] <- list(str_subset(names(L2Obs[[i]]), "[^(UUID)(Wavelength)]"))
        # }
        #
        # Variables <- stack(Variables) %>%
        #   rename(Instruments = ind, Variables = values) %>%
        #   relocate(Instruments, Variables) %>%
        #   mutate(NEWCOL=NA) %>% data.frame
        #
        # VarList(Variables)

      }
    )

    output$Hierarchy <- renderUI({

      req(VarList())

      Hierarchy=names(VarList())
      Hierarchy=head(Hierarchy,-1)
      selectizeInput(
        ns("Hierarchy"),
        "Tree Hierarchy",
        choices = Hierarchy,multiple=T,selected = Hierarchy,
        options=list(plugins=list('drag_drop','remove_button')))
    })

    network <- reactiveValues()

    observeEvent(input$d3x_update,{

      browser()

      network$nodes <- unlist(input$d3x_update$.nodesData)
      activeNode<-input$d3x_update$.activeNode
      if(!is.null(activeNode)) network$click <- jsonlite::fromJSON(activeNode)
    })

    observeEvent(
      once = TRUE,
      input$d3x_update,
      {
        network$FirstNodes <- input$d3x_update$.nodesData
      }
    )

    observeEvent(
      network$click,
      {

        output$clickView<-renderTable({
          as.data.frame(network$click)
        },caption='Last Clicked Node',caption.placement='top')
      }
    )

    TreeStruct=eventReactive(
      network$nodes,
      {

        df=VarList()
        if(is.null(network$nodes)){
          df=VarList()
        }else{

          x.filter=tree.filter(network$nodes,VarList())
          df=plyr::ddply(
            x.filter,
            "ID",
            function(a.x){
              VarList()%>%filter_(.dots = list(a.x$FILTER))%>%distinct
            })
        }
        df
      }
    )

    observeEvent(
      input$Hierarchy,
      {
        output$d3x <- renderD3tree({
          if(is.null(input$Hierarchy)){
            p=VarList()
          }else{
            p=VarList()%>%select(one_of(c(input$Hierarchy,"NEWCOL")))%>%unique
          }

          d3tree(
            data = list(
              root = df2tree(
                struct = p,
                rootname = 'x'),
              layout = 'collapse'),
            activeReturn = c('name','value','depth','id'),
            height = 18)
        })
      }
    )

    observeEvent(
      network$nodes,
      {

        output$results <- renderPrint({
          str.out=''
          if(!is.null(network$nodes)) str.out=tree.filter(network$nodes,VarList())
          return(str.out)
        })
      }
    )


    output$table <- renderTable(expr = {
      TreeStruct()%>%select(-NEWCOL)
    })

    # output$VarY <- renderUI({
    #   req(yList)
    #   selectizeInput(
    #     ns("VarY"),
    #     "Select a y variable",
    #     choices = yList(),
    #     selected = NULL,
    #     multiple = F)
    # })

    # VarX <- eventReactive(
    #   ignoreNULL = T,
    #   req(network$click$value == "Variables"),
    #   {
    #     browser()
    #
    #     VarX <- network$click$name
    #     VarXid <- network$click$id
    #
    #     raw <- input$d3x_update$.nodesData
    #     old <- network$FirstNodes
    #
    #
    #     parsed <- tidyjson::spread_all(network$FirstNodes[[1]][["children"]])
    #
    #     network$FirstNodes %>% gather_object %>% json_types
    #     Instruments <- network$FirstNodes %>% enter_object(children) %>% gather_array %>% spread_all() %>%
    #       rename(InstName = name, InstID = id)
    #
    #     #Variables <-
    #
    #     test <- Instruments %>% gather_object() %>% json_types %>% select(InstName,InstID,name,type,..JSON) %>%
    #       filter(name == "_children") %>% mutate(name = 'children')
    #
    #     test2 <- test %>% gather_array %>% spread_all
    #
    #
    #     xNode <- stack(network$nodes) #input$d3x_update$.nodesData
    #
    #     xNew <- jsonlite::fromJSON(input$d3x_update$.nodesNew)$children
    #
    #     L2Obs[["HOCR"]][[VarX]]
    #
    #
    #   }
    # )

    output$Plot <- renderPlotly({
      req(nrow(L2Obs$Metadata != 0))
      req(VarX())
      validate(need(VarX(), message = "Need x and y variables"))

      browser()

      VarX

      L2Obs[[]]


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
