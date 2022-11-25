#' parse_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_parse_mtelog_ui(ns("parse_mtelog"))
  )
}

#' parse_data Server Functions
#'
#' @noRd
mod_parse_data_server <- function(id, SearTbl, CalData, Apla){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    DataFiles <- mod_load_data_server("load_data", SearTbl)

    mod_parse_mtelog_server("parse_mtelog", SearTbl, CalData, Apla)

  })
}

## To be copied in the UI
# mod_parse_data_ui("parse_data")

## To be copied in the server
# mod_parse_data_server("parse_data")
