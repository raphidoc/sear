#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinymanager keyring lubridate listviewer gsw pracma
#' @noRd
# define some credentials
app_server <- function(input, output, session) {

# Global objects ----------------------------------------------------------

  #format(L1$SBE19()$DateTime, "%Y-%m-%d %H:%M:%OS3")
  options(digits.secs = 3)
  options(shiny.maxRequestSize=50*1024^2) # 50MB
  options(shiny.reactlog = TRUE)

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

  DB <- mod_manage_db_server("manage_db", SearTbl, Obs)

  L1a <- mod_parse_data_server("parse_data", SearTbl, CalData, MainLog)

  L1aSelect <- mod_L1a_select_server("L1a_select", MainLog, DB, Obs, ManObs, L1a)

  L1b <- mod_L1b_process_server("L1b_process", L1a, L1aSelect, CalData, Obs)

  L2 <- mod_L1bL2_server("L1bL2", Obs)

  ManObs <- mod_manage_obs_server("manage_obs", DB, L2, L1aSelect, SelObs, Obs)

  SelObs <- mod_L2_select_server("L2_select", DB, ManObs, Obs)
}
