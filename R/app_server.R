#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


# Global objects ----------------------------------------------------------

  Apla <- reactiveVal()

  Obs <- reactiveValues(
    Metadata = reactive(tibble(NA)),
    HOCR = reactiveValues(
      L1b = reactive(tibble()),
      L2 = reactive(tibble())
    )
  )

  SearTbl <- mod_manage_project_server("manage_project")

  DB <- mod_manage_DB_server("manage_DB", SearTbl, SelData)

  DataFiles <- mod_load_mtelog_server("load_mtelog", SearTbl)

  L1 <- mod_parse_mtelog_server("parse_mtelog", SearTbl, DataFiles, Apla)

  mod_filter_cut_server("filter_cut", SearTbl, DataFiles, SelData, Apla)

  SelData <- mod_select_data_server("select_data", Apla, DB)

  CalData <- mod_load_cal_server("HOCRCal")

  L1b <- mod_process_L1b_server("process_L1b", L1, SelData, CalData, Obs)

  L2 <- mod_obs_L1L2_server("obs_L1L2", L1b, Obs)

  mod_manage_obs_server("manage_obs", DB, L2, SelData, Obs)




}
