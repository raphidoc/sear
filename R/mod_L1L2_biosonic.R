#' L1L2_biosonic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_biosonic_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("L1b"), height = 320),
    # actionButton(ns("ProcessL2"), "Process L2"),
    # plotlyOutput(ns("L2"), height = 250)
  )
}

#' L1L2_biosonic Server Functions
#'
#' @noRd
mod_L1L2_biosonic_server <- function(id, Obs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$L1b <- renderPlotly({
      validate(need(nrow(Obs$BioSonic$L1b) != 0, "No L1b data"))

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

      ply <- Obs$BioSonic$L1b %>%
        plot_ly(
          text = ~ID,
          x = ~DateTime
        ) %>%
        add_lines(
          y = ~Altitude_mReMsl,
          color = "Watercraft"
        ) %>%
        add_lines(
          y = ~BottomElevation_m+PlantHeight_m,
          color = "Canopy"
        ) %>%
        add_lines(
          y = ~BottomElevation_m,
          color = "Bottom"
        ) %>%
        layout(
          shapes = BlackSquare#,
          #yaxis = list(range = list(~min(BottomElevation_m, na.rm = TRUE), 0))
        )

      # Iframe to render svg properly
      widgetframe::frameableWidget(ply)

    })

  })
}

## To be copied in the UI
# mod_L1L2_biosonic_ui("L1L2_biosonic")

## To be copied in the server
# mod_L1L2_biosonic_server("L1L2_biosonic")
