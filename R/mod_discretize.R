#' discretize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_discretize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("Discretize"), "Discretize", icon = icon("fa-solid fa-puzzle-piece", lib = "font-awesome"))
  )
}

#' discretize Server Functions
#'
#' @noRd
mod_discretize_server <- function(id, Apla) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(
      input$Discretize,
      {

      }
    )
  })
}

## To be copied in the UI
# mod_discretize_ui("discretize")

## To be copied in the server
# mod_discretize_server("discretize")
