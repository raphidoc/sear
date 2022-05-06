#' L1L2_transect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_L1L2_transect_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' L1L2_transect Server Functions
#'
#' @noRd 
mod_L1L2_transect_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_L1L2_transect_ui("L1L2_transect_1")
    
## To be copied in the server
# mod_L1L2_transect_server("L1L2_transect_1")
