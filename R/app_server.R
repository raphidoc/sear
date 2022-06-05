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

  SelData <- mod_select_data_server("select_data", L1$Apla)

  CalData <- mod_load_cal_server("HOCRCal")

  L1b <- mod_process_L1b_server("process_L1b", SelData, L1, CalData)

  observeEvent( L1b$ProcessL1b(), {

    req(L1b$Data())

    if (L1b$ObsType() == "Station") {

      mod_L1L2_station_server("L1L2_station", L1b)

    } else {

      stop(L1b$ObsType()," not implemented")

    }
  })





}
