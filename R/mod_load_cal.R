#' load_cal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_cal_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' load_cal Server Functions
#'
#' @noRd
mod_load_cal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # HOCR Calibration reactive value
    CalHOCR <- reactiveVal({
      tidy_cal_hocr()
    })

    CalSBE19 <- reactiveVal({
      read_sbe19_cal()
    })

    CalSBE18 <- reactiveVal({
      read_sbe18_cal()
    })

    CalSBE43 <- reactiveVal({
      read_sbe43_cal()
    })

    CalSeaOWL <- reactiveVal({
      read_seaowl_cal()
    })

    CalBBFL2 <- reactiveVal({
      read_bbfl2_cal()
    })


# Module output -----------------------------------------------------------
  list(
    CalHOCR = CalHOCR,
    CalSBE19 = CalSBE19,
    CalSBE18 = CalSBE18,
    CalSBE43 = CalSBE43,
    CalSeaOWL = CalSeaOWL,
    CalBBFL2 = CalBBFL2
  )

  })
}

## To be copied in the UI
# mod_load_cal_ui("load_cal_1")

## To be copied in the server
# mod_load_cal_server("load_cal_1")
