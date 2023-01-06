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

  )
}

#' bottom_reflectance Server Functions
#'
#' @noRd
mod_bottom_reflectance_server <- function(id, Obs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_bottom_reflectance_ui("bottom_reflectance")

## To be copied in the server
# mod_bottom_reflectance_server("bottom_reflectance")
