#' obs_ctd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_obs_ctd_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' obs_ctd Server Functions
#'
#' @noRd
mod_obs_ctd_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_obs_ctd_ui("obs_ctd_1")

## To be copied in the server
# mod_obs_ctd_server("obs_ctd_1")
