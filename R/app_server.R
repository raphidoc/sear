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

  L2Obs <- reactiveValues()

  ActiveMenu <- reactiveVal(list("Settings"))

  observeEvent(
    {
      input$ActiveMenu
    },{
      ActiveMenu(append(ActiveMenu(), input$ActiveMenu))
    })

# Modules logic -----------------------------------------------------------

  SearProj <- mod_manage_project_server("manage_project")

  Settings <- mod_settings_server("settings", SearProj, ActiveMenu)

  CalData <- mod_parse_cal_server("parse_cal", SearProj$History)

  DB <- mod_manage_db_server("manage_db", SearProj$History, Obs)

  L1a <- mod_parse_data_server("parse_data", SearProj$History, CalData, MainLog)

  L1aSelect <- mod_L1a_select_server("L1a_select", MainLog, DB, Obs, ManObs, L1a)

  L1b <- mod_L1b_process_server("L1b_process", L1a, L1aSelect, CalData, Obs)

  L2 <- mod_L1bL2_server("L1bL2", Obs, Settings)

  ManObs <- mod_manage_obs_server("manage_obs", DB, L2, L1aSelect, L2Select, Obs, L2Obs)

  L2Select <- mod_L2_select_server("L2_select", DB, ManObs, L2Obs)

  session$onSessionEnded(stopApp)
}
