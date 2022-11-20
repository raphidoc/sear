#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #format(L1$SBE19()$DateTime, "%Y-%m-%d %H:%M:%OS3")
  options(digits.secs = 3)
  options(shiny.reactlog = TRUE)

  # Global objects ----------------------------------------------------------

  Apla <- reactiveVal()

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
    ),
    SBE19 = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    BBFL2 = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    SeaOWL = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    )
  )


# Modules logic -----------------------------------------------------------

  SearTbl <- mod_manage_project_server("manage_project")

  DB <- mod_manage_db_server("manage_db", SearTbl, SelData, Obs)

  DataFiles <- mod_load_data_server("load_data", SearTbl)

  L1 <- mod_parse_mtelog_server("parse_mtelog", SearTbl, DataFiles, CalData, Apla)

  mod_filter_trim_server("filter_trim", SearTbl, DataFiles, SelData, Apla)

  SelData <- mod_select_data_server("select_data", Apla, DB, Obs, ManObs)

  mod_discretize_server("discretize", Apla)

  CalData <- mod_load_cal_server("CalData")

  L1b <- mod_L1b_process_server("L1b_process", L1, SelData, CalData, Obs)

  L2 <- mod_L1L2_server("L1L2", L1b, Obs)

  ManObs <- mod_manage_obs_server("manage_obs", DB, L2, SelData, Obs)
}
