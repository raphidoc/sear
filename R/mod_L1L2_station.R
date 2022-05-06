#' L1L2_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_station_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(outputId = ns("HOCRL2"))

  )
}

#' L1L2_station Server Functions
#'
#' @noRd
mod_L1L2_station_server <- function(id, Station){

  stopifnot(is.reactive(Station))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$HOCRL2 <- renderUI({

      req(Station())


      plotlyOutput(ns("HOCRL2plot"), height = 500)
      #DT::dataTableOutput()

    })

    output$HOCRL2plot <- renderPlotly({

      req(Station())


      PlyFont <- list(family="Times New Roman", size = 18)
      BlackSquare <- list(type = "rect", fillcolor = "transparent",line = list(width = 0.5), xref = "paper", yref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 1 )

      PlotHOCR <- Station() %>% select(Instrument, SN, AproxData)

      ply <- PlotHOCR %>%
        #filter(str_detect(Instrument, "HPL")) %>%
        mutate(Plot = purrr::map2(.x = AproxData, .y = SN, ~
                                    plot_ly(.x) %>%
                                    add_lines(x = ~Wavelength, y = ~Channels , name = .y, showlegend = F) %>%
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
        config(mathjax = "local", displayModeBar = T)

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)
    })

  })
}

## To be copied in the UI
# mod_L1L2_station_ui("L1L2_station")

## To be copied in the server
# mod_L1L2_station_server("L1L2_station")
