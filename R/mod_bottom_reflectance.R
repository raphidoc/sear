#' bottom_reflectance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bottom_reflectance_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("Rb"), height = 320)
  )
}

#' bottom_reflectance Server Functions
#'
#' @noRd
mod_bottom_reflectance_server <- function(id, Obs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$Rb <- renderPlotly({

      validate(need(nrow(Obs$HOCR$L2) != 0, "No HOCR L2 data"))
      validate(need(nrow(Obs$BioSonic$L2) != 0, "No BioSonic L2 data"))

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

      #browser()

      KZ <- Obs$HOCR$L2$KLu_loess * Obs$BioSonic$L2$BottomElevation_m

      #Rb <- Obs$HOCR$L2$Rrs/exp(1)^-KZ

      Obs$HOCR$L2 <- Obs$HOCR$L2 %>%
        mutate(
          Rb = Rrs/exp(1)^(-KZ)
        )

      ply <- Obs$HOCR$L2 %>% plot_ly() %>%
        add_lines(
          x = ~Wavelength,
          y = ~Rb,
          showlegend = F,
        )

      # Set source for selection event
      ply$x$source <- "Rb"

      # Iframe to render svg properly
      widgetframe::frameableWidget(ply)
    })

  })
}

## To be copied in the UI
# mod_bottom_reflectance_ui("bottom_reflectance")

## To be copied in the server
# mod_bottom_reflectance_server("bottom_reflectance")
