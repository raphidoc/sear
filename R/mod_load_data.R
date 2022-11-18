#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("LoadData"))
  )
}

#' load_data Server Functions
#'
#' @noRd
mod_load_data_server <- function(id, SearTbl) {
  stopifnot(is.reactive(SearTbl))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$LoadData <- renderUI({
      req(SearTbl())

      dropdownButton(
        mod_load_mtelog_ui(ns("load_mtelog")),
        circle = F,
        label = "Load data",
        tootltip = "path to go here"
      )
    })

    mod_load_mtelog_server("load_mtelog", SearTbl)

  })
}

## To be copied in the UI
# mod_load_data_ui("load_data")

## To be copied in the server
# mod_load_data_server("load_data")
