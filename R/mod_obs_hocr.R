#' obs_hocr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_obs_hocr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("HOCRL1b"), height = 320),
    actionButton(ns("ProcessL2"), "ProcessL2"),
    plotlyOutput(ns("AOPs"), height = 250)
  )
}

#' obs_hocr Server Functions
#'
#' @noRd
mod_obs_hocr_server <- function(id, L1bData, Obs) {
  # stopifnot(is.reactive(L1bData))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # HOCR tab ----------------------------------------------------------------
    # output$HOCR <- renderUI({
    #
    #   req(L1bData())
    #
    #   tagList(
    #     plotlyOutput(ns("HOCRL1b"), height = 320),
    #     actionButton(ns("ProcessL2"), "ProcessL2"),
    #     plotlyOutput(ns("AOPs"), height = 250)
    #   )
    # })

    # QC flag for HOCR --------------------------------------------------------
    QCData <- reactiveVal({
      reactive({
        L1bData()$HOCR$AproxData[[1]] %>%
          select(DateTime, ID) %>%
          unique() %>%
          mutate(QC = "1")
      })
    })

    # Get the ID of HOCR spectra selected in: selected()$customdata

    observeEvent(
      event_data("plotly_click", source = "HOCRL1b"),
      label = "QC HOCR",
      ignoreInit = TRUE,
      {
        Selected <- event_data("plotly_click", source = "HOCRL1b")$customdata

        tmp <- QCData()

        # Change value for selected spectrum ID
        if (tmp$QC[tmp$ID %in% Selected] == "1") {
          tmp$QC[tmp$ID %in% Selected] <- 0
        } else if (tmp$QC[tmp$ID %in% Selected] == "0") {
          tmp$QC[tmp$ID %in% Selected] <- 1
        }

        QCData(tmp)
      }
    )

    L1bHOCR <- reactive({
      L1bData()$HOCR %>%
        select(Instrument, SN, AproxData) %>%
        mutate(AproxData = purrr::map(AproxData, ~ left_join(., QCData(), by = c("DateTime", "ID"))))
    })

    # HOCR Es and Lu plot -----------------------------------------------------
    output$HOCRL1b <- renderPlotly({
      # req(L1bHOCR())
      # req(QCData())

      browser()

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
        Obs$HOCR$L2 <- L2_hocr(L1bHOCR())
      }
    )

    output$AOPs <- renderPlotly({
      # req(L2Data())

      browser()

      Rrsplot <- Obs$HOCR$L2() %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~Rrs, showlegend = F)

      KLuplot <- Obs$HOCR$L2 %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~KLu, showlegend = F)

      subplot(Rrsplot, KLuplot)
    })

    # Module output -----------------------------------------------------------

    list(
      L1bHOCR = L1bHOCR
      # AOPs = Obs$HOCR$L2
    )
  })
}

## To be copied in the UI
# mod_obs_hocr_ui("obs_hocr")

## To be copied in the server
# mod_obs_hocr_server("obs_hocr")
