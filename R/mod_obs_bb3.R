#' obs_bb3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_obs_bb3_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' obs_bb3 Server Functions
#'
#' @noRd
mod_obs_bb3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_obs_bb3_ui("obs_bb3_1")

## To be copied in the server
# mod_obs_bb3_server("obs_bb3_1")
