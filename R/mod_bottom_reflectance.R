#' bottom_reflectance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bottom_reflectance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("Rb"), height = 320)
  )
}

#' bottom_reflectance Server Functions
#'
#' @noRd
mod_bottom_reflectance_server <- function(id, Obs) {
  moduleServer(id, function(input, output, session) {
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

      #
      # KZ <- Obs$HOCR$L2
      # test <- KZ %>%
      #   mutate(
      #     KZ = KLu_loess*(Obs$BioSonic$L2$BottomElevation_m),
      #     eKZ = exp(-KZ),
      #     Rw = pi*Rrs_loess,
      #     Rb = Rw/eKZ
      #     )

      KZ <- Obs$HOCR$L2$KLu_loess * (Obs$BioSonic$L2$BottomElevation_m)

      Obs$HOCR$L2 <- isolate(Obs$HOCR$L2) %>%
        mutate(
          BRI = pi * Rrs_loess / exp(-KZ)
        )

      pal <- c("Rw" = "turquoise", "BRI" = "orange")

      ply <- Obs$HOCR$L2 %>%
        plot_ly(
          colors = pal
        ) %>%
        add_lines(
          x = ~Wavelength,
          y = ~BRI,
          showlegend = T,
          color = "BRI"
        ) %>% # add_lines(
        #   x = ~Wavelength,
        #   y = ~RbII,
        #   showlegend = T,
        #   name = "RbII",
        # ) %>%
        add_lines(
          x = ~Wavelength,
          y = ~ pi * Rrs_loess,
          showlegend = T,
          color = "Rw",
          fill = "tonexty"
        ) %>%
        layout(
          shapes = BlackSquare,
          # yaxis =list(range=c(0,1)),
          xaxis = list(range = c(400, 700))
        )

      # Set source for selection event
      ply$x$source <- "BRI"

      # Save graph
      # save_image(ply, file=file.path(path.expand("~"), "sear_figure", "BRI.svg"), scale = 3, height = 720, width = 1280)

      # Iframe to render svg properly
      widgetframe::frameableWidget(ply)
    })
  })
}

## To be copied in the UI
# mod_bottom_reflectance_ui("bottom_reflectance")

## To be copied in the server
# mod_bottom_reflectance_server("bottom_reflectance")
