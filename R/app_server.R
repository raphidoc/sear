#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny lubridate gsw pracma
#' @noRd
# define some credentials
app_server <- function(input, output, session) {

  # # Use keyring package to set keys
  # keyring::key_set("user", "sear")
  #
  # # Init DB using credentials data
  # credentials <- data.frame(
  #   user = c("sear"),
  #   password = keyring::key_get("user", "sear"),
  #   # password will automatically be hashed
  #   admin = c(TRUE),
  #   stringsAsFactors = FALSE
  # )
  #
  # keyring::key_set("database", "sear")
  #
  # # Init the database
  # shinymanager::create_db(
  #   credentials_data = credentials,
  #   sqlite_path = "~/sear_project/database.sqlite", # will be created
  #   passphrase = keyring::key_get("database", "sear")
  # )

  # call the server part
  # check_credentials returns a function to authenticate users
  # res_auth <- shinymanager::secure_server(
  #   check_credentials = check_credentials(
  #     "~/sear_project/database.sqlite",
  #     passphrase = key_get("database", "sear")
  #   ),
  #   keep_token = TRUE
  # )
  #
  # observe(message(getwd()))
  #
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })

# Global objects ----------------------------------------------------------

  #format(L1$SBE19()$DateTime, "%Y-%m-%d %H:%M:%OS3")
  options(digits.secs = 3)
  options(shiny.maxRequestSize=50*1024^2) # 50MB
  options(shiny.reactlog = TRUE)

  MainLog <- reactiveVal(tibble())

  # Active discrete observation
  Obs <- reactiveValues(
    MetadataL2 = tibble(
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
      Altitude = numeric(),
      SolZen = numeric(),
      SolAzm = numeric(),
      BoatSolAzm = numeric(),
      Comment = character(),
      UUID = character()
    ),
    MetadataL1b = tibble(),
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

  AutoProcess <- mod_automatic_processing_server(
    "automatic_processing",
    L1a, L1aSelect, CalData, Obs, Settings, MainLog, DB)

  L1b <- mod_L1b_process_server("L1b_process", L1a, L1aSelect, CalData, Obs, MainLog, Settings)

  L2 <- mod_L1bL2_server("L1bL2", Obs, Settings)

  ManObs <- mod_manage_obs_server("manage_obs", DB, L2, L1aSelect, L2Select, Obs, L2Obs)

  L2Select <- mod_L2_select_server("L2_select", DB, ManObs, L2Obs)

  session$onSessionEnded(stopApp)
}
