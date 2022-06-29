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

  SearTbl <- mod_manage_project_server("manage_project")

  DB <- mod_manage_DB_server("manage_DB", SearTbl, SelData)

  DataFiles <- mod_load_mtelog_server("load_mtelog", SearTbl)

  L1 <- mod_parse_mtelog_server("parse_mtelog", SearTbl, DataFiles)

  SelData <- mod_select_data_server("select_data", L1$Apla, DB)

  CalData <- mod_load_cal_server("HOCRCal")

  L1b <- mod_process_L1b_server("process_L1b", L1, SelData, CalData)

  L2 <- mod_station_L1L2_server("station_L1L2", L1b, Station)

  Obs <- mod_manage_obs_server("manage_obs", DB, L2, SelData, Station)

  Station <- reactiveValues(
    Metadata = reactive(tibble(NA)),
    HOCR = reactiveValues(
      L1b = reactive(tibble()),
      L2 = reactive(tibble())
    )
  )


  # observeEvent(
  #   req(L1b$ProcessL1b() & L1b$ObsType() == "Station"),
  #   {
  #     L2 <- mod_station_L1L2_server("station_L1L2", L1b)
  #   })

  # observeEvent(
  #   L1b$ProcessL1b(),
  #   {
  #
  #     req(L1b$Data())
  #
  #     if (L1b$ObsType() == "Station") {
  #
  #       L2 <- mod_station_L1L2_server("station_L1L2", L1b)
  #
  #       Obs <- mod_manage_obs_server("manage_obs", DB, L2, SelData)
  #
  #     } else {
  #
  #       stop(L1b$ObsType()," not implemented")
  #
  #     }
  #   })

}
