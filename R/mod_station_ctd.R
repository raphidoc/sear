#' station_ctd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_station_ctd_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' station_ctd Server Functions
#'
#' @noRd 
mod_station_ctd_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_station_ctd_ui("station_ctd_1")
    
## To be copied in the server
# mod_station_ctd_server("station_ctd_1")
