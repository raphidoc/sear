#' settings UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("WaveSeq")),
    uiOutput(ns("PositionHOCR"))
  )
}

#' settings Server Functions
#'
#' @noRd
mod_settings_server <- function(id, SearProj, ActiveMenu) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    LastSettings <- reactiveVal()

    observeEvent(
      {
        SearProj$Con()
      },
      {
        DBI::dbSendStatement(
          SearProj$Con(),
          "CREATE TABLE IF NOT EXISTS `Settings` (
          `WaveMin` REAL,
          `WaveMax` REAL,
          `WaveStep` REAL,
          `Z1Depth` REAL,
          `Z1Z2Depth` REAL,
          `date_time` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        LastSettings(tibble(DBI::dbGetQuery(SearProj$Con(), "SELECT * FROM Settings ORDER BY date_time DESC LIMIT 1;")))
      }
    )

    observeEvent(
      {
        c(
          isTruthy(
            ActiveMenu()[[length(ActiveMenu()) - 1]] == "Settings" &&
              last(ActiveMenu() != "Settings")
          )
        )
      },
      {
        req(SearProj$Con())

        NewSettings <- tibble(
          WaveMin = input$WaveMin,
          WaveMax = input$WaveMax,
          WaveStep = input$WaveStep,
          Z1Depth = input$Z1Depth,
          Z1Z2Depth = input$Z1Z2Depth,
          date_time = as.character(as.POSIXlt(Sys.time(), tz = "utc")),
          uuid_l2 = SearProj$Accessuuid_l2()
        )

        DBI::dbWriteTable(SearProj$Con(), "Settings", NewSettings, append = TRUE)
      }
    )

    output$WaveSeq <- renderUI({
      req(SearProj$History())


      WaveMin <- ifelse(
        identical(LastSettings()$WaveMin, as.numeric()),
        356, LastSettings()$WaveMin
      )

      WaveMax <- ifelse(
        identical(LastSettings()$WaveMax, as.numeric()),
        800, LastSettings()$WaveMax
      )

      WaveStep <- ifelse(
        identical(LastSettings()$WaveStep, as.numeric()),
        3, LastSettings()$WaveStep
      )

      tagList(
        numericInput(
          ns("WaveMin"),
          "Minimum wavelength [nm]",
          WaveMin,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        ),
        numericInput(
          ns("WaveMax"),
          "Maximum wavelength [nm]",
          WaveMax,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        ),
        numericInput(
          ns("WaveStep"),
          "wavelength Step [nm]",
          WaveStep,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        )
      )
    })

    # TODO: Validate input unit (meter) based on reasonable value assumption.
    # For example if input = 15, user probably want to say 0.15 ... from personal experience

    output$PositionHOCR <- renderUI({
      req(SearProj$History())

      Z1Depth <- ifelse(
        identical(LastSettings()$Z1Depth, as.numeric()),
        NA, LastSettings()$Z1Depth
      )

      Z1Z2Depth <- ifelse(
        identical(LastSettings()$Z1Z2Depth, as.numeric()),
        NA, LastSettings()$Z1Z2Depth
      )

      tagList(
        numericInput(
          ns("Z1Depth"),
          "LuZ1Depth [m]",
          Z1Depth,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        ),
        numericInput(
          ns("Z1Z2Depth"),
          "Depth difference LuZ1-LuZ2 [m]",
          Z1Z2Depth,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        )
      )
    })

    # Module output -----------------------------------------------------------

    Settings <- reactiveValues(
      HOCR = reactiveValues(
        WaveMin = reactive(input$WaveMin),
        WaveMax = reactive(input$WaveMax),
        WaveStep = reactive(input$WaveStep),
        Z1Depth = reactive(input$Z1Depth),
        Z1Z2Depth = reactive(input$Z1Z2Depth),
      )
    )
  })
}

## To be copied in the UI
# mod_settings_ui("settings")

## To be copied in the server
# mod_settings_server("settings")
