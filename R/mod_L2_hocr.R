#' hocr_l2 UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hocr_l2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("AOPs"), height = 250)
  )
}

#' hocr_l2 Server Functions
#'
#' @noRd
mod_hocr_l2_server <- function(id, L2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    PlyFont <- list(family = "times New Roman", size = 18)
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
      # req(l2_data())

      validate(need(nrow(L2) != 0, "Process L2 to display AOPs"))

      Rrsplot <- L2 %>%
        plot_ly() %>%
        add_lines(x = ~wavelength, y = ~Rrs, showlegend = F)

      KLuplot <- L2 %>%
        plot_ly() %>%
        add_lines(x = ~wavelength, y = ~KLu, showlegend = F)

      subplot(Rrsplot, KLuplot, shareX = T) %>%
        add_annotations(
          text = ~ TeX("\\text{wavelength [nm]}"),
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
          # xaxis3 = list(title = list(text = TeX("\\text{wavelength}")))
        ) %>%
        config(mathjax = "cdn", displayModeBar = T)
    })
  })
}

## To be copied in the UI
# mod_hocr_l2_ui("hocr_l2")

## To be copied in the server
# mod_hocr_l2_server("hocr_l2")
