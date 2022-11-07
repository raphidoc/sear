#' L1L2_obs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_obs_L1L2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("TabPanel"))
  )
}

#' L1L2_obs Server Functions
#'
#' @noRd
mod_obs_L1L2_server <- function(id, L1b, Obs) {
  # stopifnot(is.reactive(L1b$Data))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns



    # Tab panel ---------------------------------------------------------------
    output$TabPanel <- renderUI({
      req(nrow(Obs$Metadata) != 0)

      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
        tabPanel(
          "Observation",
          uiOutput(ns("ObsTab"))
        ),
        tabPanel(
          "HOCR",
          plotlyOutput(ns("HOCRL1b"), height = 320),
          actionButton(ns("ProcessL2"), "Process L2"),
          plotlyOutput(ns("AOPs"), height = 250)
          # mod_obs_hocr_ui(ns("obs_hocr"))
        )
      )
    })

    # Keep that for dev purpose

    # observeEvent(
    #   req(ObsTbl()),
    #   {
    #     appendTab(inputId ="Tabset", tabPanel("Obs", uiOutput(ns("Obs"))))
    #   })
    #
    # observeEvent(
    #   req(str_detect(names(L1b$Data()), "HOCR")),
    #   {
    #     appendTab(inputId ="Tabset", tabPanel("HOCR", mod_obs_hocr_ui(ns("obs_hocr"))))
    #   })

    # Obs tab -------------------------------------------------------------
    ObsTbl <- reactiveVal({})

    observeEvent(
      L1b$ProcessL1b(), # This one create even if L1b processing fail
      # Obs$HOCR$L1b, # This one doesnt update if L1b doesn't change
      {
        Obs$Metadata <- tibble(
          ObsName = "NA",
          ObsType = "NA",
          ObsFlag = "NA",
          DateTime = as.character(mean(L1b$SelApla()$DateTime, na.rm = T)),
          DateTimeMin = as.character(min(L1b$SelApla()$DateTime, na.rm = T)),
          DateTimeMax = as.character(max(L1b$SelApla()$DateTime, na.rm = T)),
          TimeElapsed = as.numeric(interval(DateTimeMin, DateTimeMax)), # in second
          Lat = mean(L1b$SelApla()$Lat_DD, na.rm = T),
          Lon = mean(L1b$SelApla()$Lon_DD, na.rm = T),
          LatMin = min_geo(L1b$SelApla()$Lat_DD, na.rm = T),
          LatMax = max_geo(L1b$SelApla()$Lat_DD, na.rm = T),
          LonMin = min_geo(L1b$SelApla()$Lon_DD, na.rm = T),
          LonMax = max_geo(L1b$SelApla()$Lon_DD, na.rm = T),
          DistanceRun = pracma::haversine(c(LatMin, LonMin), c(LatMax, LonMax)) * 1000, # in meter
          BoatSolAzm = mean(L1b$SelApla()$BoatSolAzm, na.rm = T),
          Comment = "NA",
          UUID = "NA"
        )
      }
    )

    # DataTable used to display Obs information
    output$DataTable <- DT::renderDataTable(
      DT::datatable(Obs$Metadata,
        extensions = c("Buttons", "Scroller", "Select"),
        # filter = "top",
        escape = TRUE, rownames = FALSE,
        style = "bootstrap",
        class = "compact",
        options = list(
          dom = "Brtip",
          select = list(style = "os", items = "row"),
          buttons = list(I("colvis"), "selectNone", "csv"),
          columnDefs = list(
            list(
              visible = FALSE,
              targets = c(0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12)
            )
          ),
          deferRender = TRUE,
          scrollY = 100,
          pageLength = 10,
          scroller = TRUE
        ),
        selection = "none",
        editable = F
      ),
      server = FALSE,
      editable = F
    )

    output$ObsTab <- renderUI({
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
        mod_manage_obs_ui("manage_obs")
      )
    })

    observeEvent(
      input$Save,
      {
        Obs$Metadata <- Obs$Metadata %>% mutate(Comment = input$Comment)
      }
    )

    # HOCR tab ----------------------------------------------------------------

    # Have to call this module inside a reactive consumer, why ?
    # Because a reactiveVal is not a reactive consumer
    # mod_obs_hocr_server("obs_hocr", L1b$Data, Obs)

    # Then have to force computation of HOCRL2 ...
    # observe({
    #   req(HOCRL2())
    #   browser()
    #   HOCRL2()
    # })


    # QC flag for HOCR --------------------------------------------------------
    # QCData <- reactive({
    #
    #   Obs$HOCR$L1b$AproxData[[1]] %>%
    #     select(DateTime, ID) %>%
    #     unique() %>%
    #     mutate(QC = "1")
    # })

    # Get the ID of HOCR spectra selected in: selected()$customdata

    observeEvent(
      event_data("plotly_click", source = "HOCRL1b"),
      label = "QC HOCR",
      ignoreInit = TRUE,
      {
        Selected <- event_data("plotly_click", source = "HOCRL1b")$customdata

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
        # Obs$HOCR$L1b <- Obs$HOCR$L1b %>%
        #   select(Instrument, SN, AproxData) %>%
        #   mutate(AproxData = purrr::map(AproxData, ~ left_join(., tmp, by = c("DateTime", "ID"))))

        qc_shift <- function(df, Selected) {
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

        Obs$HOCR$L1b <- Obs$HOCR$L1b %>% mutate(AproxData = purrr::map(AproxData, ~ qc_shift(., Selected)))
      }
    )

    # L1bHOCR <- reactive({
    #
    #   Obs$HOCR$L1b %>%
    #     select(Instrument, SN, AproxData) %>%
    #     mutate(AproxData = purrr::map(AproxData, ~ left_join(., QCData(), by = c("DateTime", "ID"))))
    #
    # })

    # HOCR Es and Lu plot -----------------------------------------------------
    output$HOCRL1b <- renderPlotly({
      # req(L1bHOCR())
      # req(QCData())

      PlyFont <- list(family = "Times New Roman", size = 18)
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

      ply <- Obs$HOCR$L1b %>%
        # filter(str_detect(Instrument, "HPL")) %>%
        mutate(
          Plot = purrr::map2(
            .x = AproxData,
            .y = SN,
            ~ plot_ly(
              .x,
              text = ~ID,
              customdata = ~ID
            ) %>%
              add_lines(
                x = ~Wavelength,
                y = ~Channels,
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
                yaxis = list(
                  rangemode = "nonnegative"
                  # title = list(text = ~paste0(unique(.x$Type), unique(.x$Units)))
                ),
                xaxis = list(rangemode = "nonnegative")
              )
          )
        )

      Lu <- ply %>%
        filter(str_detect(Instrument, "HPL")) %>%
        subplot(shareX = T, shareY = T)

      Es <- ply %>%
        filter(str_detect(Instrument, "HSE"))

      Es <- Es$Plot

      p <- subplot(Es[[1]], Lu, nrows = 2, margin = 0.035) %>%
        add_annotations(
          text = ~ TeX("\\text{Wavelength [nm]}"),
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
          yaxis2 = list(title = list(text = TeX("\\text{L}_\\text{u}"))) # ,
          # xaxis3 = list(title = list(text = TeX("\\text{Wavelength}")))
        ) %>%
        config(mathjax = "local", displayModeBar = T) %>%
        event_register("plotly_click")

      # Set source for selection event
      p$x$source <- "HOCRL1b"

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)
    })

    # HOCR AOPs computation ---------------------------------------------------
    observeEvent(
      input$ProcessL2,
      {
        Obs$HOCR$L2 <- L2_hocr(Obs$HOCR$L1b)
      }
    )

    output$AOPs <- renderPlotly({
      # req(L2Data())

      validate(need(nrow(Obs$HOCR$L2) != 0, "Process L2 to display AOPs"))

      Rrsplot <- Obs$HOCR$L2 %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~Rrs, showlegend = F)

      KLuplot <- Obs$HOCR$L2 %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~KLu, showlegend = F)

      subplot(Rrsplot, KLuplot)
    })

    # BB3 tab -----------------------------------------------------------------


    # Module output -----------------------------------------------------------

    list(
      # Save = reactive(input$Save),
      # Delete = reactive(input$Delete)
      # ObsTbl = ObsTbl,
      # HOCR = HOCRL2
    )
  })
}

## To be copied in the UI
# mod_L1L2_obs_ui("L1L2_obs")

## To be copied in the server
# mod_L1L2_obs_server("L1L2_obs")
