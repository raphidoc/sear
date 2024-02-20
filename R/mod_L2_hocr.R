#' L2_hocr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L2_hocr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("AOPs"), height = 250)
  )
}

#' L2_hocr Server Functions
#'
#' @noRd
mod_L2_hocr_server <- function(id, L2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    output$AOPs <- renderPlotly({
      # req(L2Data())

      validate(need(nrow(L2) != 0, "Process L2 to display AOPs"))

      Rrsplot <- L2 %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~Rrs, showlegend = F)

      KLuplot <- L2 %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~KLu, showlegend = F)

      subplot(Rrsplot, KLuplot, shareX = T) %>%
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
          yaxis = list(title = list(
            text = "Rrs" # TeX("\\text{R}_\\text{rs}")
          )),
          yaxis2 = list(title = list(
            text = "Klu" # TeX("\\text{K}_\\text{Lu}")
          )) # ,
          # xaxis3 = list(title = list(text = TeX("\\text{Wavelength}")))
        ) %>%
        config(mathjax = "cdn", displayModeBar = T)
    })
  })
}

## To be copied in the UI
# mod_L2_hocr_ui("L2_hocr")

## To be copied in the server
# mod_L2_hocr_server("L2_hocr")
