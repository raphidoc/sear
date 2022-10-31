#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


# Global objects ----------------------------------------------------------

  Apla <- reactiveVal()

  BBFL2 <- reactiveVal()

  SeaOWL <- reactiveVal()

  SBE19 <- reactiveVal()

  # Active discrete observation
  Obs <- reactiveValues(
    Metadata = tibble(
      ObsName = character(),
      ObsType = character(),
      ObsFlag = character(),
      DateTime = character(),
      DateTimeMin = character(),
      DateTimeMax = character(),
      TimeElapsed = numeric(),
      Lat = numeric(),
      Lon = numeric(),
      LatMin = numeric(),
      LatMax = numeric(),
      LonMin = numeric(),
      LonMax = numeric(),
      DistanceRun = numeric(),
      BoatSolAzm = numeric(),
      Comment = character(),
      UUID = character()
    ),
    HOCR = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    )
  )

  SearTbl <- mod_manage_project_server("manage_project")

  DB <- mod_manage_DB_server("manage_DB", SearTbl, SelData, Obs)

  DataFiles <- mod_load_mtelog_server("load_mtelog", SearTbl)

  L1 <- mod_parse_mtelog_server("parse_mtelog", SearTbl, DataFiles, CalData, Apla, BBFL2, SeaOWL, SBE19)

  mod_filter_trim_server("filter_trim", SearTbl, DataFiles, SelData, Apla)

  SelData <- mod_select_data_server("select_data", Apla, DB, Obs, ManObs)

  mod_discretize_server("discretize", Apla)

  CalData <- mod_load_cal_server("HOCRCal")

  L1b <- mod_process_L1b_server("process_L1b", L1, SelData, CalData, Obs)

  L2 <- mod_obs_L1L2_server("obs_L1L2", L1b, Obs)

  ManObs <- mod_manage_obs_server("manage_obs", DB, L2, SelData, Obs)

}
