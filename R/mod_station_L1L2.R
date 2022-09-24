#' L1L2_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_station_L1L2_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("TabPanel"))
  )
}

#' L1L2_station Server Functions
#'
#' @noRd
mod_station_L1L2_server <- function(id, L1b, Station){

  #stopifnot(is.reactive(L1b$Data))

  moduleServer( id, function(input, output, session){
    ns <- session$ns



# Tab panel ---------------------------------------------------------------
    output$TabPanel <- renderUI({
      req(Station)

      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
        tabPanel(
          "Station",
          uiOutput(ns("Station"))
        ),
        tabPanel("HOCR",
                 plotlyOutput(ns("HOCRL1b"), height = 320),
                 actionButton(ns("ProcessL2"), "ProcessL2"),
                 plotlyOutput(ns("AOPs"), height = 250)
                 #mod_station_hocr_ui(ns("station_hocr"))
                 )
      )

    })

    # Keep that for dev purpose

    # observeEvent(
    #   req(StationTbl()),
    #   {
    #     appendTab(inputId ="Tabset", tabPanel("Station", uiOutput(ns("Station"))))
    #   })
    #
    # observeEvent(
    #   req(str_detect(names(L1b$Data()), "HOCR")),
    #   {
    #     appendTab(inputId ="Tabset", tabPanel("HOCR", mod_station_hocr_ui(ns("station_hocr"))))
    #   })

# Station tab -------------------------------------------------------------
    StationTbl <- reactiveVal({})

    observeEvent(
      L1b$ProcessL1b(),
      {
          Station$Metadata <- tibble(
            ObsName = L1b$ObsName(),
            ObsType = L1b$ObsType(), # Obviously should be "Station"
            DateTime = mean(L1b$SelApla()$DateTime, na.rm = T),
            Lat = mean(L1b$SelApla()$Lat_DD, na.rm = T),
            Lon = mean(L1b$SelApla()$Lon_DD, na.rm = T)
          )

      }
    )

    #DataTable used to display Station information
    output$DataTable <- DT::renderDataTable(
      DT::datatable(Station$Metadata,
                    #extensions = c("Buttons", "Scroller", "Select"),
                    #filter = "top",
                    escape = TRUE, rownames = FALSE,
                    style = "bootstrap",
                    class = "compact",
                    options = list(
                      #dom = "Brtip",
                      #select = list(style = 'os', items = 'row'),
                      #buttons = list(I("colvis"),"selectNone","csv"),
                      #columnDefs = list(
                       # list(
                          #visible = FALSE,
                          #targets = c(0,2,3)
                        #)),
                      deferRender = TRUE,
                      scrollY = 100,
                      pageLength = 10,
                      scroller = TRUE
                    ),
                    selection = "none",
                    editable = F
      ),
      server=FALSE,
      editable=T
    )

    output$Station <- renderUI({

      tagList(
        DT::DTOutput(ns("DataTable")),
        textAreaInput(
          ns("Comment"),
          "Comment",
          value = "No comment",
          width = NULL,
          height = NULL,
          cols = NULL,
          rows = NULL,
          placeholder = NULL,
          resize = NULL
        ),

        actionButton(ns("Delete"), "Delete", icon = icon("glyphicon glyphicon-trash", lib = "glyphicon")),
        actionButton(ns("Save"), "Save", icon = icon("glyphicon glyphicon-save", lib = "glyphicon"))
      )
    })

    observeEvent(
      input$Save,
      {
        Station$Metadata <- Station$Metadata() %>% mutate(Comment = input$Comment)
      })

# HOCR tab ----------------------------------------------------------------

    # Have to call this module inside a reactive consumer, why ?
    # Because a reactiveVal is not a reactive consumer
    #mod_station_hocr_server("station_hocr", L1b$Data, Station)

    # Then have to force computation of HOCRL2 ...
    # observe({
    #   req(HOCRL2())
    #   browser()
    #   HOCRL2()
    # })


    # QC flag for HOCR --------------------------------------------------------
    QCData <- reactive({

      browser()
      Station$HOCR$L1b$AproxData[[1]] %>%
        select(DateTime, ID) %>%
        unique() %>%
        mutate(QC = "1")
    })

    # Get the ID of HOCR spectra selected in: selected()$customdata

    observeEvent(
      event_data('plotly_click', source = "HOCRL1b"),
      label = "QC HOCR",
      ignoreInit = TRUE,
      {
        Selected <- event_data('plotly_click', source = "HOCRL1b")$customdata

        # tmp <- QCData()
        #
        # # Change value for selected spectrum ID
        # if (tmp$QC[tmp$ID %in% Selected] == "1") {
        #   tmp$QC[tmp$ID %in% Selected] <- 0
        # } else if (tmp$QC[tmp$ID %in% Selected] == "0") {
        #   tmp$QC[tmp$ID %in% Selected] <- 1
        # }
        #
        # #QCData <- reactive(tmp)
        #
        # Station$HOCR$L1b <- Station$HOCR$L1b %>%
        #   select(Instrument, SN, AproxData) %>%
        #   mutate(AproxData = purrr::map(AproxData, ~ left_join(., tmp, by = c("DateTime", "ID"))))


        qc_shift <- function(df, Selected){

          df %>%
            filter(ID == Selected) %>%
            mutate(QC = if_else(QC == "1", "0", "1")) %>%
            bind_rows(df %>% filter(ID != Selected))

          # . %>% mutate(QC = case_when(
          #   QC == "1" ~ "0",
          #   QC == "0" ~ "1"
          # ))

          # if (.$QC[.$ID %in% Selected] == "1") {
          #   .$QC[.$ID %in% Selected] <- 0
          # } else if (.$QC[.$ID %in% Selected] == "0") {
          #   .$QC[.$ID %in% Selected] <- 1
          # }

        }

        Station$HOCR$L1b <- Station$HOCR$L1b %>% mutate(AproxData = purrr::map(AproxData, ~ qc_shift(., Selected)))
      }
    )

    # L1bHOCR <- reactive({
    #
    #   Station$HOCR$L1b %>%
    #     select(Instrument, SN, AproxData) %>%
    #     mutate(AproxData = purrr::map(AproxData, ~ left_join(., QCData(), by = c("DateTime", "ID"))))
    #
    # })

    # HOCR Es and Lu plot -----------------------------------------------------
    output$HOCRL1b <- renderPlotly({

      #req(L1bHOCR())
      #req(QCData())

      #browser()

      PlyFont <- list(family="Times New Roman", size = 18)
      BlackSquare <- list(
        type = "rect",
        fillcolor = "transparent",
        line = list(width = 0.5),
        xref = "paper",
        yref = "paper",
        x0 = 0,
        x1 = 1,
        y0 = 0,
        y1 = 1
      )

      ply <- Station$HOCR$L1b %>%
        #filter(str_detect(Instrument, "HPL")) %>%
        mutate(
          Plot = purrr::map2(
            .x = AproxData,
            .y = SN,
            ~plot_ly(
              .x,
              text = ~ID,
              customdata = ~ID
            ) %>%
              add_lines(
                x = ~Wavelength,
                y = ~Channels ,
                name = ~QC,
                showlegend = F,
                color = ~QC,
                colors = c("1" = "seagreen", "0" = "red")
              ) %>%
              add_annotations(
                text = ~.y,
                x = 0.5,
                y = 1,
                yref = "paper",
                xref = "paper",
                xanchor = "middle",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 15)
              ) %>%
              layout(
                shapes = BlackSquare,
                yaxis = list(rangemode = "nonnegative"
                             #title = list(text = ~paste0(unique(.x$Type), unique(.x$Units)))
                ),
                xaxis = list(rangemode = "nonnegative")
              )
          ))

      Lu <- ply %>%
        filter(str_detect(Instrument, "HPL")) %>%
        subplot(shareX = T, shareY = T)

      Es <- ply %>%
        filter(str_detect(Instrument, "HSE"))

      Es <- Es$Plot

      p <- subplot(Es[[1]], Lu, nrows = 2, margin = 0.035) %>%
        add_annotations(
          text = ~TeX("\\text{Wavelength [nm]}"),
          x = 0.5,
          y = -0.1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 18)
        ) %>%
        layout(
          font = PlyFont,
          yaxis = list(title = list(text = TeX("\\text{E}_\\text{s}"))),
          yaxis2 = list(title = list(text = TeX("\\text{L}_\\text{u}")))#,
          #xaxis3 = list(title = list(text = TeX("\\text{Wavelength}")))
        ) %>%
        config(mathjax = "local", displayModeBar = T) %>%
        event_register('plotly_click')

      # Set source for selection event
      p$x$source <- "HOCRL1b"

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)
    })

    # HOCR AOPs computation ---------------------------------------------------
    observeEvent(
      input$ProcessL2,
      {
        Station$HOCR$L2 <- L2_hocr(Station$HOCR$L1b)
      }
    )

    output$AOPs <- renderPlotly({

      #req(L2Data())

      #browser()

      Rrsplot <- Station$HOCR$L2 %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~Rrs, showlegend = F)

      KLuplot <- Station$HOCR$L2 %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~KLu, showlegend = F)

      subplot(Rrsplot, KLuplot)
    })

# BB3 tab -----------------------------------------------------------------


# Module output -----------------------------------------------------------

  list(
    Save = reactive(input$Save),
    Delete = reactive(input$Delete)
    #StationTbl = StationTbl,
    #HOCR = HOCRL2
  )

   })
}

## To be copied in the UI
# mod_L1L2_station_ui("L1L2_station")

## To be copied in the server
# mod_L1L2_station_server("L1L2_station")
