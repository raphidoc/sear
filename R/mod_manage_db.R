#' manage_db UI Function
#'
#' @description a shiny Module.
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
    uiOutput(outputId = ns("ensemble_list")),
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
        uuid_l2 = character(),
        ensemble = numeric(),
        lat = numeric(),
        lon = numeric(),
        distance_run = numeric(),
        date_time = character(),
        time_elapsed = character()
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

        # metadata_l1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS metadata_l1b (
          ensemble DOUBLE,
          date_time DOUBLE,
          speed DOUBLE,
          lon DOUBLE,
          lat DOUBLE,
          altitude DOUBLE,
          sol_zen DOUBLE,
          sol_azi DOUBLE,
          boat_raa DOUBLE,
          roll DOUBLE,
          pitch DOUBLE,
          heading DOUBLE,
          boat_xx DOUBLE,
          boat_xy DOUBLE,
          boat_xz DOUBLE,
          boat_yx DOUBLE,
          boat_yy DOUBLE,
          boat_yz DOUBLE,
          boat_zx DOUBLE,
          boat_zy DOUBLE,
          boat_zz DOUBLE,
          uuid_l2 TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          );"
        )

        # metadata_l2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS metadata_l2 (
          ensemble DOUBLE NOT NULL,
          date_time TEXT NOT NULL,
          date_time_min TEXT NOT NULL,
          date_time_max TEXT NOT NULL,
          time_elapsed DOUBLE NOT NULL,
          speed DOUBLE NOT NULL,
          lon DOUBLE NOT NULL,
          lat DOUBLE NOT NULL,
          lon_min DOUBLE NOT NULL,
          lon_max DOUBLE NOT NULL,
          lat_min DOUBLE NOT NULL,
          lat_max DOUBLE NOT NULL,
          distance_run DOUBLE NOT NULL,
          altitude DOUBLE NOT NULL,
          sol_zen DOUBLE,
          sol_azi DOUBLE,
          boat_raa DOUBLE,
          roll DOUBLE,
          pitch DOUBLE,
          heading DOUBLE,
          qwip_score DOUBLE,
          boat_xx DOUBLE,
          boat_xy DOUBLE,
          boat_xz DOUBLE,
          boat_yx DOUBLE,
          boat_yy DOUBLE,
          boat_yz DOUBLE,
          boat_zx DOUBLE,
          boat_zy DOUBLE,
          boat_zz DOUBLE,
          comment TEXT,
          uuid_l2 TEXT PRIMARY KEY,
          date_time_processing TEXT NOT NULL,
          analyst TEXT NOT NULL,
          mail TEXT NOT NULL
          );"
        )

        # hocr_l1a
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `hocr_l1a` (
          instrument,
          sn,
          time,
          integration_time,
          sample_delay,
          dark_sample,
          dark_average,
          spectrometer_temperature,
          frame,
          timer,
          checksum,
          channel,
          uuid_l2,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # hocr_l1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `hocr_l1b` (
          `instrument` TEXT,
          `sn` TEXT,
          id,
          qc,
          date_time,
          integration_time,
          sample_delay,
          dark_sample,
          dark_average,
          spectrometer_temperature,
          frame,
          timer,
          checksum,
          type,
          wavelength,
          channel,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # hocr_l2
        # cat(paste(names(hocr_l2), collapse = ",\n"))
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `hocr_l2` (
          wavelength,
          rrs_median,
          rrs_mad,
          lw_median,
          lw_mad,
          klu_median,
          klu_mad,
          klu_luz1_rel_unc,
          klu_luz2_rel_unc,
          klu_rel_unity,
          lw_luz1_rel_unc,
          lw_luz2_rel_unc,
          lw_z1_rel_unc,
          lw_temp_rel_unc,
          lw_sal_rel_unc,
          lw_rel_unity,
          rrs_luz1_rel_unc,
          rrs_luz2_rel_unc,
          rrs_z1_rel_unc,
          rrs_temp_rel_unc,
          rrs_sal_rel_unc,
          rrs_es_rel_unc,
          rrs_rel_unity,
          uuid_l2,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # hocr OPTIC3 cal
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `OPTIC3` (
          instrument,
          sn,
          type,
          wavelength,
          units,
          field_length,
          data_type,
          cal_lines,
          fit_type,
          a0,
          a1,
          im,
          cint
          )"
        )

        # hocr THERM1 cal
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `THERM1` (
          instrument,
          sn,
          type,
          id,
          units,
          field_length,
          data_type,
          cal_lines,
          fit_type,
          m0,
          m1,
          m2,
          m3,
          Tr
          )"
        )

        # hocr INTTIME cal
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `INTTIME` (
          instrument,
          sn,
          type,
          id,
          units,
          field_length,
          data_type,
          cal_lines,
          fit_type,
          a0,
          a1
          )"
        )

        # hocr SAMPLE cal
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `SAMPLE` (
          instrument,
          sn,
          type,
          id,
          units,
          field_length,
          data_type,
          cal_lines,
          fit_type,
          a0,
          a1
          )"
        )

        # `wavelength` REAL,
        # `Rrs` REAL,
        # `Rrs_loess` REAL,
        # `KLu` REAL,
        # `KLu_loess` REAL,
        # `RbI` REAL,
        # `uuid_l2` TEXT,

        # SBE19 L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `sbe19_l1b` (
          `parameter` TEXT,
          `date_time` TEXT,
          `id` INTEGER,
          `qc` TEXT,
          `value` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # SBE19 L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `sbe19_l2` (
          `oxygen_solubility` REAL,
          `oxygen_concentration` REAL,
          `ph` REAL,
          `pressure` REAL,
          `salinity_absolute` REAL,
          `salinity_practical` REAL,
          `temperature` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # SeaOWL L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `seaowl_l1b` (
          `parameter` TEXT,
          `date_time` TEXT,
          `id` INTEGER,
          `qc` TEXT,
          `value` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # SeaOWL L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `seaowl_l2` (
          `vsf_700` REAL,
          `chl` REAL,
          `fdom` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # BBFL2 L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `bbfl2_l1b` (
          `parameter` TEXT,
          `date_time` TEXT,
          `id` INTEGER,
          `qc` TEXT,
          `value` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # BBFL2 L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `bbfl2_l2` (
          `ntu` REAL,
          `pc` REAL,
          `pe` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `biosonic_l1b` (
          `lon` REAL,
          `lat` REAL,
          `date_time` REAL,
          `altitude_m` REAL,
          `bottom_elevation_m` REAL,
          `plant_height_m` REAL,
          `percent_coverage` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `biosonic_l2` (
          `lon` REAL,
          `lat` REAL,
          `date_time` REAL,
          `altitude_m` REAL,
          `bottom_elevation_m` REAL,
          `plant_height_m` REAL,
          `percent_coverage` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `hydroball_l1b` (
          `lon` REAL,
          `lat` REAL,
          `date_time` REAL,
          `altitude` REAL,
          `height_watercolumn` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        # BioSonic L2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `hydroball_l2` (
          `lon` REAL,
          `lat` REAL,
          `date_time` REAL,
          `altitude` REAL,
          `height_watercolumn` REAL,
          `uuid_l2` TEXT,
          FOREIGN KEY (uuid_l2)
            REFERENCES metadata_l2 (uuid_l2)
            ON DELETE CASCADE
          )"
        )

        ObsMeta(tibble(DBI::dbGetQuery(Con, "SELECT * FROM metadata_l2")))

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
    #         # if DB is not empty list uuid_l2 and get current index
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
    #             uuid_l2 = character(),
    #             lat = numeric(),
    #             lon = numeric(),
    #             date_time = character(),
    #           )
    #         }
    #       })
    #     })


# Search by uuid_l2 ----------------------------------------------------------

    output$ObsList <- renderUI({
      req(Con())
      validate(need(nrow(ObsMeta()) != 0, message = "Empty DB"))

      selectizeInput(
        ns("ObsList"),
        "ObsList",
        multiple = FALSE,
        choices = NULL,
        selected = NULL,
        options = list(
          create = FALSE,
          placeholder = "Search My uuid_l2",
          maxItems = "1",
          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
          onType = I("function (str) {if (str === \"\") {this.close();}}")
        )
      )
    })

    observeEvent(
      ObsMeta()$uuid_l2,
      {
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "ObsList", choices = c("", ObsMeta()$uuid_l2), server = T
        )
      }
    )

# Search by ensemble ------------------------------------------------------

    output$ensemble_list <- renderUI({
      req(Con())
      validate(need(nrow(ObsMeta()) != 0, message = "Empty DB"))

      selectizeInput(
        ns("ensemble_list"),
        "ensemble_list",
        multiple = FALSE,
        choices = NULL,
        selected = NULL,
        options = list(
          create = FALSE,
          placeholder = "Search by ensemble",
          maxItems = "1",
          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
          onType = I("function (str) {if (str === \"\") {this.close();}}")
        )
      )
    })

  observeEvent(
    ObsMeta()$ensemble,
    {
      updateSelectizeInput(
        session = getDefaultReactiveDomain(),
        "ensemble_list", choices = c("", ObsMeta()$ensemble), server = T
      )
    }
  )

  ObsSel <- reactiveVal()

  observeEvent(
      list(input$ObsList, input$ensemble_list),
      ignoreInit = T,
      ignoreNULL = T,
    {

      if (input$ObsList != "") {
        ObsSel(input$ObsList)
      }

      if (input$ensemble_list != "") {
        ObsSel(
          ObsMeta()$uuid_l2[which(ObsMeta()$ensemble == input$ensemble_list)]
          )
      }
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

        #         "SELECT date_time, lat, lon, wavelength, Rrs FROM Metadata
        #           LEFT JOIN hocr_l2 ON Metadata.uuid_l2 = hocr_l2.uuid_l2;"
        #
        #         "SELECT Metadata.date_time, Metadata.lat, Metadata.lon, speed, time_elapsed, altitude, distance_run, boat_raa, qwip_score, wavelength, Rrs, KLu, altitude_m, bottom_elevation_m, plant_height_m, percent_coverage, oxygen_concentration, ph, salinity_absolute, salinity_practical, temperature, vsf_700, chl, fdom FROM Metadata
        # LEFT JOIN hocr_l2 ON Metadata.uuid_l2 = hocr_l2.uuid_l2
        # LEFT JOIN biosonic_l2 ON Metadata.uuid_l2 = biosonic_l2.uuid_l2
        # LEFT JOIN sbe19_l2 ON Metadata.uuid_l2 = sbe19_l2.uuid_l2
        # LEFT JOIN seaowl_l2 ON Metadata.uuid_l2 = seaowl_l2.uuid_l2;"
        #
        #         long <- read_csv("/D/Documents/Algae-WISE_jet-ski_Rrs.csv")
        #         Wide <- long %>%
        #           pivot_wider(
        #             names_prefix = "Rrs_",
        #             names_from = wavelength,
        #             values_from = Rrs
        #           )
        #         write_csv(Wide, "/D/Documents/Algae-WISE_jet-ski_Rrs_wide.csv")
      }
    )

    list(
      ObsMeta = ObsMeta,
      ObsSel = ObsSel,
      Con = Con
    )
  })
}

## To be copied in the UI
# mod_manage_db_ui("manage_db")

## To be copied in the server
# mod_manage_db_server("manage_db")
