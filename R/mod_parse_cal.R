#' load_cal UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_cal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_load_cal_ui(ns("HOCR")),
    mod_load_cal_ui(ns("SBE19")),
    mod_load_cal_ui(ns("SBE18")),
    mod_load_cal_ui(ns("SBE43")),
    mod_load_cal_ui(ns("SeaOWL")),
    mod_load_cal_ui(ns("BBFL2"))
  )
}

#' load_cal Server Functions
#'
#' @noRd
mod_parse_cal_server <- function(id, SearProj, DB) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hocr_cal <- reactiveVal()
    CalSBE19 <- reactiveVal()
    CalSBE18 <- reactiveVal()
    CalSBE43 <- reactiveVal()
    CalSeaOWL <- reactiveVal()
    CalBBFL2 <- reactiveVal()

    ParsedCalFiles <- eventReactive(
      {
        SearProj()
      },
      {
        CalDir <- file.path(SearProj()$ProjPath, "sear", "cal")

        if (dir.exists(CalDir)) {
          list.files(CalDir, full.names = TRUE)
        } else {
          FALSE
        }
      }
    )

    mod_load_cal_server(
      "HOCR",
      SearProj,
      tidy_cal_hocr,
      hocr_cal,
      ParsedCalFiles,
      DB
    )

    mod_load_cal_server(
      "SBE19",
      SearProj,
      read_sbe19_cal,
      CalSBE19,
      ParsedCalFiles
    )

    mod_load_cal_server(
      "SBE18",
      SearProj,
      read_sbe18_cal,
      CalSBE18,
      ParsedCalFiles
    )

    mod_load_cal_server(
      "SBE43",
      SearProj,
      read_sbe43_cal,
      CalSBE43,
      ParsedCalFiles
    )

    mod_load_cal_server(
      "SeaOWL",
      SearProj,
      read_seaowl_cal,
      CalSeaOWL,
      ParsedCalFiles
    )

    mod_load_cal_server(
      "BBFL2",
      SearProj,
      read_bbfl2_cal,
      CalBBFL2,
      ParsedCalFiles
    )

    # Module output -----------------------------------------------------------
    list(
      hocr_cal = hocr_cal,
      CalSBE19 = CalSBE19,
      CalSBE18 = CalSBE18,
      CalSBE43 = CalSBE43,
      CalSeaOWL = CalSeaOWL,
      CalBBFL2 = CalBBFL2
    )
  })
}

## To be copied in the UI
# mod_parse_cal_ui("parse_cal")

## To be copied in the server
# mod_parse_cal_server("parse_cal")
