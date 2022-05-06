#' load_cal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_cal_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' load_cal Server Functions
#'
#' @noRd
mod_load_cal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # HOCR Calibration reactive value
    reactiveVal({
      tidy_cal_hocr()
    })

  })
}

## To be copied in the UI
# mod_load_cal_ui("load_cal_1")

## To be copied in the server
# mod_load_cal_server("load_cal_1")
