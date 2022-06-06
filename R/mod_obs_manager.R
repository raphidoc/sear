#' obs_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_obs_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' obs_manager Server Functions
#'
#' @noRd 
mod_obs_manager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_obs_manager_ui("obs_manager_1")
    
## To be copied in the server
# mod_obs_manager_server("obs_manager_1")
