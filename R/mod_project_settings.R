#' project_settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_project_settings_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' project_settings Server Functions
#'
#' @noRd
mod_project_settings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_project_settings_ui("project_settings")

## To be copied in the server
# mod_project_settings_server("project_settings")
