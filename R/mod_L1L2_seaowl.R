#' obs_seaowl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_seaowl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("SeaOWLL1b"))
  )
}

#' obs_seaowl Server Functions
#'
#' @noRd
mod_L1L2_seaowl_server <- function(id, Obs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$SeaOWLL1b <- renderPlotly({

      SeaOWLL1bNest <- Obs$SeaOWL$L1b %>%
        select(!any_of(c("SN")))%>%
        pivot_longer(
          cols = any_of(c("Bb_700", "Chl", "FDOM")),
          names_to = "Parameter",
          values_to = "Value"
        ) %>%
        group_by(Parameter) %>%
        nest()


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


      SeaOWLL1bNest <- SeaOWLL1bNest %>%
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

      subplot(SeaOWLL1bNest$Plot, nrows = nrow(SeaOWLL1bNest), shareX = TRUE, titleY = TRUE)
    })


  })
}

## To be copied in the UI
# mod_L1L2_seaowl_ui("L1L2_seaowl")

## To be copied in the server
# mod_L1L2_seaowl_server("L1L2_seaowl")
