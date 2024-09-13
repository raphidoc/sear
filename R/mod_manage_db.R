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
    uiOutput(outputId = ns("ObsList")),
    downloadButton(ns("DownloadDB"), "Download DB"),
    uiOutput(outputId = ns("ExportDB"))
  )
}

#' manage_db Server Functions
#'
#' @noRd
mod_manage_db_server <- function(id, SearProj, Obs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ObsMeta <- reactiveVal({
      tibble(
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
      req(SearProj()$ProjPath),
      {
        message("Doing L1bL2 SQLite stuff ... you know")

        L2Dir <- file.path(SearProj()$ProjPath, "L2")

        PotSQLite <- file.path(L2Dir, paste0(SearProj()$ProjName, "_sear.sqlite"))

        if (!dir.exists(L2Dir)) {
          dir.create(L2Dir)
        }

        Con <- DBI::dbConnect(RSQLite::SQLite(), PotSQLite, extended_types = TRUE)

        # Enable foreign keys
        DBI::dbExecute(conn = Con, "PRAGMA foreign_keys=ON")

        # Create DB schema
        # MetadataL2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS MetadataL2 (
          ensemble DOUBLE NOT NULL,
          DateTime TEXT NOT NULL,
          DateTimeMin TEXT NOT NULL,
          DateTimeMax TEXT NOT NULL,
          TimeElapsed DOUBLE NOT NULL,
          Speed DOUBLE NOT NULL,
          Lon DOUBLE NOT NULL,
          Lat DOUBLE NOT NULL,
          LonMin DOUBLE NOT NULL,
          LonMax DOUBLE NOT NULL,
          LatMin DOUBLE NOT NULL,
          LatMax DOUBLE NOT NULL,
          DistanceRun DOUBLE NOT NULL,
          Altitude DOUBLE NOT NULL,
          SolZen DOUBLE,
          SolAzm DOUBLE,
          BoatSolAzm DOUBLE,
          Roll DOUBLE,
          Pitch DOUBLE,
          Heading DOUBLE,
          ScoreQWIP DOUBLE,
          VesselXx DOUBLE,
          VesselXy DOUBLE,
          VesselXz DOUBLE,
          VesselYx DOUBLE,
          VesselYy DOUBLE,
          VesselYz DOUBLE,
          VesselZx DOUBLE,
          VesselZy DOUBLE,
          VesselZz DOUBLE,
          Comment TEXT,
          UUID TEXT PRIMARY KEY,
          ProTime TEXT NOT NULL,
          Analyst TEXT NOT NULL,
          Mail TEXT NOT NULL
          );"
        )

        # MetadataL1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS MetadataL1b (
          ensemble DOUBLE,
          DateTime DOUBLE,
          Speed DOUBLE,
          Lon DOUBLE,
          Lat DOUBLE,
          Altitude DOUBLE,
          SolZen DOUBLE,
          SolAzm DOUBLE,
          BoatSolAzm DOUBLE,
          Roll DOUBLE,
          Pitch DOUBLE,
          Heading DOUBLE,
          VesselXx DOUBLE,
          VesselXy DOUBLE,
          VesselXz DOUBLE,
          VesselYx DOUBLE,
          VesselYy DOUBLE,
          VesselYz DOUBLE,
          VesselZx DOUBLE,
          VesselZy DOUBLE,
          VesselZz DOUBLE,
          UUID TEXT,
          FOREIGN KEY (UUID)
            REFERENCES MetadataL2 (UUID)
            ON DELETE CASCADE
          );"
        )

        # HOCRL1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HOCRL1b` (
          `Instrument` TEXT,
          `SN` TEXT,
          ID,
          QC,
          DateTime,
          IntTime,
          SampleDelay,
          DarkSample,
          DarkAverage,
          SpecTemp,
          Frame,
          Timer,
          CheckSum,
          Type,
          Wavelength,
          Channels,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES MetadataL2 (UUID)
            ON DELETE CASCADE
          )"
        )

        # HOCRL2
        # cat(paste(names(HOCRL2), collapse = ",\n"))
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HOCRL2` (
          wavelength REAL,
          rrs_mean REAL,
          rrs_sd REAL,
          lw_mean REAL,
          lw_sd REAL,
          klu_mean REAL,
          klu_sd REAL,
          klu_luz1_rel_unc REAL,
          lw_luz1_rel_unc REAL,
          rrs_luz1_rel_unc REAL,
          klu_luz2_rel_unc REAL,
          lw_luz2_rel_unc REAL,
          rrs_luz2_rel_unc REAL,
          lw_z1_rel_unc REAL,
          rrs_z1_rel_unc REAL,
          rrs_es_rel_unc REAL,
          klu_rel_unity REAL,
          lw_rel_unity REAL,
          rrs_rel_unity REAL,
          UUID TEXT,
          FOREIGN KEY (UUID)
            REFERENCES MetadataL2 (UUID)
            ON DELETE CASCADE
          )"
        )

        # `Wavelength` REAL,
        # `Rrs` REAL,
        # `Rrs_loess` REAL,
        # `KLu` REAL,
        # `KLu_loess` REAL,
        # `RbI` REAL,
        # `UUID` TEXT,

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
            REFERENCES MetadataL2 (UUID)
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
            REFERENCES MetadataL2 (UUID)
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
            REFERENCES MetadataL2 (UUID)
            ON DELETE CASCADE
          )"
        )

        # SeaOWL L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `SeaOWLL2` (
          `VSF_700` REAL,
          `Chl` REAL,
          `FDOM` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES MetadataL2 (UUID)
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
            REFERENCES MetadataL2 (UUID)
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
            REFERENCES MetadataL2 (UUID)
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
            REFERENCES MetadataL2 (UUID)
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
            REFERENCES MetadataL2 (UUID)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HydroBallL1b` (
          `Lon` REAL,
          `Lat` REAL,
          `DateTime` REAL,
          `Altitude` REAL,
          `H` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES MetadataL2 (UUID)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HydroBallL2` (
          `Lon` REAL,
          `Lat` REAL,
          `DateTime` REAL,
          `Altitude` REAL,
          `H` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES MetadataL2 (UUID)
            ON DELETE CASCADE
          )"
        )

        ObsMeta(tibble(DBI::dbGetQuery(Con, "SELECT * FROM MetadataL2")))

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

      # selectizeInput(
      #   ns("ObsList"),
      #   "ObsList",
      #   choices = NULL,
      #   selected = NULL,
      #   multiple = F)

      selectizeInput(
        ns("ObsList"),
        "ObsList",
        multiple = FALSE,
        choices = NULL,
        selected = NULL,
        options = list(
          create = FALSE,
          placeholder = "Search My UUID",
          maxItems = "1",
          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
          onType = I("function (str) {if (str === \"\") {this.close();}}")
        )
      )
    })

    observeEvent(
      ObsMeta()$UUID,
      {
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "ObsList", choices = c("", ObsMeta()$UUID), server = T
        )
      }
    )

    output$ExportDB <- renderUI({
      req(Con())
      validate(need(nrow(ObsMeta()) != 0, message = "Empty DB"))

      actionButton(ns("ExportDB"), "Export DB")
    })


    # Download SQLite DB ------------------------------------------------------

    output$DownloadDB <- downloadHandler(
      filename = function() {
        basename(Con()@dbname)
      },
      content = function(file) {
        file.copy(Con()@dbname, file)
      }
    )

    observeEvent(
      input$ExportDB,
      {
        browser()

        #         "SELECT DateTime, Lat, Lon, Wavelength, Rrs FROM Metadata
        #           LEFT JOIN HOCRL2 ON Metadata.UUID = HOCRL2.UUID;"
        #
        #         "SELECT Metadata.DateTime, Metadata.Lat, Metadata.Lon, Speed, TimeElapsed, Altitude, DistanceRun, BoatSolAzm, ScoreQWIP, Wavelength, Rrs, KLu, Altitude_mReMsl, BottomElevation_m, PlantHeight_m, PercentCoverage, Oxygen, pH, SA, SP, Temperature, VSF_700, Chl, FDOM FROM Metadata
        # LEFT JOIN HOCRL2 ON Metadata.UUID = HOCRL2.UUID
        # LEFT JOIN BioSonicL2 ON Metadata.UUID = BioSonicL2.UUID
        # LEFT JOIN SBE19L2 ON Metadata.UUID = SBE19L2.UUID
        # LEFT JOIN SeaOWLL2 ON Metadata.UUID = SeaOWLL2.UUID;"
        #
        #         Long <- read_csv("/D/Documents/Algae-WISE_jet-ski_Rrs.csv")
        #         Wide <- Long %>%
        #           pivot_wider(
        #             names_prefix = "Rrs_",
        #             names_from = Wavelength,
        #             values_from = Rrs
        #           )
        #         write_csv(Wide, "/D/Documents/Algae-WISE_jet-ski_Rrs_wide.csv")
      }
    )

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
