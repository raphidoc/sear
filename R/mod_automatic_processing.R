#' automatic_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_automatic_processing_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' automatic_processing Server Functions
#'
#' @noRd
mod_automatic_processing_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_automatic_processing_ui("automatic_processing")

## To be copied in the server
# mod_automatic_processing_server("automatic_processing")
