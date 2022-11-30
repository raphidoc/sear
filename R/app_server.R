#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #format(L1$SBE19()$DateTime, "%Y-%m-%d %H:%M:%OS3")
  options(digits.secs = 3)
  options(shiny.maxRequestSize=50*1024^2) # 50MB
  options(shiny.reactlog = TRUE)

  # Global objects ----------------------------------------------------------

  MainLog <- reactiveVal(tibble())

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
      Altitude = numeric(),
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
    SeaOWL = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    BBFL2 = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    BioSonic = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    )
  )


# Modules logic -----------------------------------------------------------

  SearTbl <- mod_manage_project_server("manage_project")

  CalData <- mod_parse_cal_server("parse_cal", SearTbl)

  DB <- mod_manage_db_server("manage_db", SearTbl, SelData, Obs)

  L1 <- mod_parse_data_server("parse_data", SearTbl, CalData, MainLog)

  SelData <- mod_select_data_server("select_data", MainLog, DB, Obs, ManObs, L1)

  L1b <- mod_L1b_process_server("L1b_process", L1, SelData, CalData, Obs)

  L2 <- mod_L1L2_server("L1L2", L1b, Obs)

  ManObs <- mod_manage_obs_server("manage_obs", DB, L2, SelData, Obs)
}
