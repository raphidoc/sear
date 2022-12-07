#' obs_hocr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_hocr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("HOCRL1b"), height = 320),
    actionButton(ns("ProcessL2"), "Process L2"),
    plotlyOutput(ns("AOPs"), height = 250)
  )
}

#' obs_hocr Server Functions
#'
#' @noRd
mod_L1L2_hocr_server <- function(id, Obs) {
  # stopifnot(is.reactive(L1bData))

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

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # HOCR Es and Lu plot
    output$HOCRL1b <- renderPlotly({
      validate(need(nrow(Obs$HOCR$L1b) != 0, "No L1b data"))



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
          x = 0.4,
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
          yaxis = list(title = list(text ="Es" #TeX("\\text{E}_\\text{s}")
                                    )),
          yaxis2 = list(title = list(text = "Lu" #TeX("\\text{L}_\\text{u}")
                                     )) # ,
          # xaxis3 = list(title = list(text = TeX("\\text{Wavelength}")))
        ) %>%
        config(mathjax = "cdn", displayModeBar = T) %>%
        event_register("plotly_click")

      # Set source for selection event
      p$x$source <- "HOCRL1b"

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)
    })

    # Get the ID of HOCR spectra selected in: selected()$customdata
    observeEvent(
      event_data("plotly_click", source = "HOCRL1b"),
      label = "QC HOCR",
      ignoreInit = TRUE,
      {
        Selected <- event_data("plotly_click", source = "HOCRL1b")$customdata

        Obs$HOCR$L1b <- Obs$HOCR$L1b %>% mutate(AproxData = purrr::map(AproxData, ~ qc_shift(., Selected)))
      }
    )

    # HOCR AOPs computation
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

      subplot(Rrsplot, KLuplot) %>%
        add_annotations(
          text = ~ TeX("\\text{Wavelength [nm]}"),
          x = 0.4,
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
          yaxis = list(title = list(text = "Rrs"#TeX("\\text{R}_\\text{rs}")
                                    )),
          yaxis2 = list(title = list(text = "Klu"#TeX("\\text{K}_\\text{Lu}")
                                     )) # ,
          # xaxis3 = list(title = list(text = TeX("\\text{Wavelength}")))
        ) %>%
        config(mathjax = "cdn", displayModeBar = T)
    })

  })
}

## To be copied in the UI
# mod_L1L2_hocr_ui("L1L2_hocr")

## To be copied in the server
# mod_L1L2_hocr_server("L1L2_hocr")
