#' obs_ctd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_sbe19_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("SBE19L1b"))
  )
}

#' obs_ctd Server Functions
#'
#' @noRd
mod_L1L2_sbe19_server <- function(id, Obs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$SBE19L1b <- renderPlotly({

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

      SBE19nest <- Obs$SBE19$L1b %>%
        select(!any_of(c("Conductivity", "CT", "O2Sol")))%>%
        pivot_longer(
          cols = any_of(c("Temperature", "Pressure", "SP", "SA", "OxSol", "Oxygen", "pH")),
          names_to = "Parameter",
          values_to = "Value"
        ) %>%
        group_by(Parameter) %>%
        nest()


      SBE19nest <- SBE19nest %>%
        mutate(
          Plot = purrr::map2(.x = data, .y = Parameter, ~
                               plot_ly(.x, text = ~ID) %>%
                               add_markers(x = ~DateTime, y = ~Value) %>%
                               layout(
                                 shapes = BlackSquare,
                                 yaxis = list(title = list(text = .y))
                               )
          )
        )

      subplot(SBE19nest$Plot, nrows = nrow(SBE19nest), shareX = TRUE, titleY = TRUE)

    })

  })
}

## To be copied in the UI
# mod_L1L2_sbe19_ui("L1L2_sbe19")

## To be copied in the server
# mod_L1L2_sbe19_server("L1L2_sbe19")
