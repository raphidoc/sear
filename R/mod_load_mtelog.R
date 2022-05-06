#' load_mtelog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_mtelog_ui <- function(id){
  ns <- NS(id)
  tagList(

    fileInput(ns("mtelog"), NULL, buttonLabel = "upload", multiple = TRUE, accept = c(".txt",".bin")),

  )
}

#' load_mtelog Server Functions
#'
#' @noRd
mod_load_mtelog_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive({
      req(input$mtelog)

      reactiveValues(
        txt = {input$mtelog$datapath[stringr::str_detect(input$mtelog$datapath, ".txt")]},
        bin = {input$mtelog$datapath[stringr::str_detect(input$mtelog$datapath, ".bin")]}
        )
    })
  })
}

## To be copied in the UI
# mod_load_mtelog_ui("load_mtelog_1")

## To be copied in the server
# mod_load_mtelog_server("load_mtelog_1")
