#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Max download size allowed in bytes (For filesInput, not needed anymore)
  # options(shiny.maxRequestSize = 10000*1024^2)

  SearTbl <- mod_project_manager_server("project_manager")

  DataFiles <- mod_load_mtelog_server("load_mtelog", SearTbl)

  L1 <- mod_parse_mtelog_server("parse_mtelog", SearTbl, DataFiles)

  SelDisplay <- mod_selection_display_server("selection_display", L1$Apla)

  mod_select_instrument_server("select_instrument", L1$MainLog)

  CalData <- mod_load_cal_server("HOCRCal")

  L1b <- mod_process_L1L2_server("process_L1L2", L1$Apla, SelDisplay$UpApla, SelDisplay$Selected, L1$HOCR, L1$TimeIndexHOCR, CalData)

  observeEvent( L1b$ProcessL1b(), {

    req(L1b$Data())

    if (L1b$ObsType() == "Station") {

      mod_L1L2_station_server("L1L2_station", L1b$Data, L1b$ObsName)

    } else {

      stop(L1b$ObsType()," not implemented")

    }
  })





}
