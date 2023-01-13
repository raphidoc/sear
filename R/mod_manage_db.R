#' manage_db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manage_db_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("ObsList"))
  )
}

#' manage_db Server Functions
#'
#' @noRd
mod_manage_db_server <- function(id, SearTbl, Obs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ObsMeta <- reactiveVal({
      tibble(
        ObsType = character(),
        ObsName = character(),
        UUID = character(),
        Lat = numeric(),
        Lon = numeric(),
        DistanceRun = numeric(),
        DateTime = character(),
        TimeElapsed = character()
      )
    })

    # Connect project SQLite DB -----------------------------------------------
    Con <- eventReactive(
      req(SearTbl()$ProjPath),
      {
        message("Doing L1bL2 SQLite stuff ... you know")

        L2Dir <- file.path(SearTbl()$ProjPath, "L2")

        PotSQLite <- file.path(L2Dir, paste0(SearTbl()$ProjName, "_sear.sqlite"))

        if (!dir.exists(L2Dir)) {
          dir.create(L2Dir)
        }

        Con <- DBI::dbConnect(RSQLite::SQLite(), PotSQLite, extended_types = TRUE)

        # Enable foreign keys
        DBI::dbExecute(conn = Con, "PRAGMA foreign_keys=ON")

        # Create DB schema
        # Metadata
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS Metadata (
          ObsName TEXT NOT NULL,
          ObsType TEXT NOT NULL,
          ObsFlag TEXT NOT NULL,
          DateTime TEXT NOT NULL,
          DateTimeMin TEXT NOT NULL,
          DateTimeMax TEXT NOT NULL,
          TimeElapsed INTEGER NOT NULL,
          Lon DOUBLE NOT NULL,
          Lat DOUBLE NOT NULL,
          LonMin DOUBLE NOT NULL,
          LonMax DOUBLE NOT NULL,
          LatMin DOUBLE NOT NULL,
          LatMax DOUBLE NOT NULL,
          Altitude DOUBLE NOT NULL,
          DistanceRun DOUBLE NOT NULL,
          BoatSolAzm DOUBLE NOT NULL,
          Comment TEXT,
          UUID TEXT PRIMARY KEY,
          ProTime TEXT NOT NULL,
          Analyst TEXT NOT NULL,
          Mail TEXT NOT NULL
          );"
        )

        # HOCRL1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HOCRL1b` (
          `Instrument` TEXT,
          `SN` TEXT,
          `DateTime` TEXT,
          `ID` INTEGER,
          `QC` TEXT,
          `Type` TEXT,
          `Wavelength` REAL,
          `Channels` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # HOCRL2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HOCRL2` (
          `Wavelength` REAL,
          `Rrs` REAL,
          `Rrs_loess` REAL,
          `KLu` REAL,
          `KLu_loess` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # SBE19 L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `SBE19L1b` (
          `Parameter` TEXT,
          `DateTime` TEXT,
          `ID` INTEGER,
          `QC` TEXT,
          `Value` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # SBE19 L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `SBE19L2` (
          `OxSol` REAL,
          `Oxygen` REAL,
          `pH` REAL,
          `Pressure` REAL,
          `SA` REAL,
          `SP` REAL,
          `Temperature` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # SeaOWL L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `SeaOWLL1b` (
          `Parameter` TEXT,
          `DateTime` TEXT,
          `ID` INTEGER,
          `QC` TEXT,
          `Value` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # SeaOWL L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `SeaOWLL2` (
          `Bb_700` REAL,
          `Chl` REAL,
          `FDOM` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # BBFL2 L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `BBFL2L1b` (
          `Parameter` TEXT,
          `DateTime` TEXT,
          `ID` INTEGER,
          `QC` TEXT,
          `Value` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # BBFL2 L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `BBFL2L2` (
          `NTU` REAL,
          `PC` REAL,
          `PE` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `BioSonicL1b` (
          `Lon` REAL,
          `Lat` REAL,
          `DateTime` REAL,
          `Altitude_mReMsl` REAL,
          `BottomElevation_m` REAL,
          `PlantHeight_m` REAL,
          `PercentCoverage` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `BioSonicL2` (
          `Lon` REAL,
          `Lat` REAL,
          `DateTime` REAL,
          `Altitude_mReMsl` REAL,
          `BottomElevation_m` REAL,
          `PlantHeight_m` REAL,
          `PercentCoverage` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )


        ObsMeta(tibble(DBI::dbGetQuery(Con, "SELECT * FROM Metadata")))

        # Return Con
        Con
      }
    )


    # Fetch MetaData ----------------------------------------------------------
    #
    #     ObsMeta <- reactiveVal({
    #       reactive({
    #         req(Con())
    #
    #         # if DB is not empty list UUID and get current index
    #         if (
    #           !identical(DBI::dbListTables(Con()), character(0)) #&
    #           #str_detect(DBI::dbListTables(Con), "ObsMeta")
    #         ) {
    #           message("Listing Obs")
    #
    #           tibble(DBI::dbGetQuery(Con(), "SELECT * FROM Metadata"))
    #
    #         } else {
    #           tibble(
    #             ObsType = character(),
    #             ObsName = character(),
    #             UUID = character(),
    #             Lat = numeric(),
    #             Lon = numeric(),
    #             DateTime = character(),
    #           )
    #         }
    #       })
    #     })

    output$ObsList <- renderUI({
      req(Con())
      validate(need(nrow(ObsMeta()) != 0, message = "Empty DB"))

      selectizeInput(ns("ObsList"), "ObsList", choices = c("", ObsMeta()$UUID), selected = NULL, multiple = F)
    })

    list(
      ObsMeta = ObsMeta,
      ObsSel = reactive(input$ObsList),
      Con = Con
    )
  })
}

## To be copied in the UI
# mod_manage_db_ui("manage_db")

## To be copied in the server
# mod_manage_db_server("manage_db")
