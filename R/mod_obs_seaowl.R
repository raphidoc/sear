#' obs_seaowl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_obs_seaowl_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' obs_seaowl Server Functions
#'
#' @noRd
mod_obs_seaowl_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_obs_seaowl_ui("obs_seaowl_1")

## To be copied in the server
# mod_obs_seaowl_server("obs_seaowl_1")
