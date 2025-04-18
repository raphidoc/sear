#' parse_data UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("TabPanel")),
    textOutput(ns("SurveyDuration"))
  )
}

#' parse_data Server Functions
#'
#' @noRd
mod_parse_data_server <- function(id, SearProj, cal_data, MainLog) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$TabPanel <- renderUI({
      req(SearProj())

      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
        tabPanel(
          "Parsed data",
          mod_select_instrument_ui(ns("select_instrument"))
        ),
        tabPanel(
          "SeaDooMTE",
          mod_parse_mtelog_ui(ns("parse_mtelog"))
        ),
        tabPanel(
          "BioSonic",
          mod_parse_biosonic_ui(ns("parse_biosonic"))
        ),
        tabPanel(
          "HB Devices",
          mod_parse_hb_devices_ui(ns("parse_hb_devices"))
        ),
        tabPanel(
          "SatView",
          mod_parse_satview_ui(ns("parse_satview"))
        )
      )
    })

    # Parsing raw files -------------------------------------------------------

    ToProcess <- mod_select_instrument_server("select_instrument", ParsedFiles)

    # SeaDoo MTE data logger configuration
    MTELogInput <- mod_parse_mtelog_server("parse_mtelog", SearProj, cal_data, ParsedFiles)
    BioSonicInput <- mod_parse_biosonic_server("parse_biosonic", SearProj, ParsedFiles)

    # HydroBall Devices configuration
    HBDevicesInput <- mod_parse_hb_devices_server("parse_hb_devices", SearProj, ParsedFiles)
    SatViewInput <- mod_parse_satview_server("parse_satview", SearProj, cal_data, ParsedFiles)

    # Reading sear parsed files -----------------------------------------------

    Apla <- reactiveVal()
    HOCR <- reactiveVal()
    hocr_dark <- reactiveVal()
    HOCRtimeIndex <- reactiveVal()
    SBE19 <- reactiveVal()
    SeaOWL <- reactiveVal()
    BBFL2 <- reactiveVal()

    HBDevices <- reactiveVal()
    BioSonic <- reactiveVal()

    # List parsed files, to be computed at project selection and on new instrument input
    ParsedFiles <- eventReactive(
      {
        c(
          SearProj(),
          MTELogInput(),
          BioSonicInput(),
          HBDevicesInput(),
          SatViewInput()
        )
      },
      {
        ParsedDir <- file.path(SearProj()$ProjPath, "sear", "data", "parsed")

        if (dir.exists(ParsedDir)) {
          list.files(ParsedDir, full.names = TRUE)
        } else {
          FALSE
        }
      }
    )

    # ParsedData is used to keep track of the loaded data against MainLog

    observeEvent(
      # ignoreInit = TRUE,
      {
        c(
          ParsedFiles()
        )
      },
      {
        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Loading data: ", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # Applanix

        progress$set(value = 0.1, detail = "Applanix")

        NameApla <- c("apla_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameApla))) {
          PotApla <- str_subset(ParsedFiles(), NameApla)

          temp <- read_csv(PotApla)

          Apla(temp)
        }

        # HOCR

        progress$set(value = 0.2, detail = "HOCR")

        NameHOCR <- c("hocr_[:digit:]{8}_[:digit:]{6}\\.rds")

        if (any(str_detect(ParsedFiles(), NameHOCR))) {
          PotHOCR <- str_subset(ParsedFiles(), NameHOCR)

          temp <- unlist(purrr::map(.x = PotHOCR, ~ read_rds(.x)), recursive = F)

          HOCR(temp)
        }

        progress$set(value = 0.3, detail = "HOCR dark")

        NameHOCRDark <- c("hocr_dark_[:digit:]{8}_[:digit:]{6}\\.rds")

        if (any(str_detect(ParsedFiles(), NameHOCRDark))) {
          PotHOCRDark <- str_subset(ParsedFiles(), NameHOCRDark)

          temp <- purrr::map(.x = PotHOCRDark, ~ read_rds(.x))

          test <- purrr::map(
            .x = temp,
            ~ tibble(
              instrument = .x$instrument,
              sn = .x$sn,
              cal_data = purrr::map(
                .x = ..1$cal_data,
                ~ pivot_longer(
                  .x,
                  cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
                  values_to = "channel",
                  names_to = c("type", "wavelength"),
                  names_sep = "_",
                  # names_prefix = "[[:alpha:]]{2}_",
                  names_transform = list(wavelength = as.numeric)
                )
              )
            )
          )

          test2 <- purrr::map(.x = test, ~ unnest(.x, cols = c(cal_data)))

          test3 <- purrr::map_df(.x = test2, ~.x) %>%
            group_by(instrument, sn) %>%
            nest(cal_data = !matches("instrument|sn")) %>%
            mutate(
              cal_data = purrr::map(
                cal_data,
                ~ pivot_wider(
                  .x,
                  names_from = all_of(c("type", "wavelength")),
                  names_sep = "_",
                  values_from = channel
                )
              )
            )

          hocr_dark(test3)
        }

        progress$set(value = 0.4, detail = "HOCR time index")

        NameHOCRtimeIndex <- c("hocr_time_index_[:digit:]{8}_[:digit:]{6}\\.rds")

        if (any(str_detect(ParsedFiles(), NameHOCRtimeIndex))) {
          PotHOCRtimeIndex <- str_subset(ParsedFiles(), NameHOCRtimeIndex)

          # unlist convert posixct date_time representation back to number of seconds
          # temp <- as.POSIXct(
          #   unlist(
          #     purrr::map(.x = PotHOCRtimeIndex, ~ read_rds(.x)),
          #     recursive = T),
          #   tz = "utc")

          temp <- unlist(
            purrr::map(.x = PotHOCRtimeIndex, ~ read_rds(.x)),
            recursive = T
          )

          HOCRtimeIndex(temp)
        }

        # SBE19

        progress$set(value = 0.5, detail = "SBE19")

        NameSBE19 <- c("sbe19_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameSBE19))) {
          PotSBE19 <- str_subset(ParsedFiles(), NameSBE19)

          SBE19(read_csv(PotSBE19))
        }

        # SeaOWL

        progress$set(value = 0.6, detail = "SeaOWL")

        NameSeaOWL <- c("seaowl_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameSeaOWL))) {
          PotSeaOWL <- str_subset(ParsedFiles(), NameSeaOWL)

          SeaOWL(read_csv(PotSeaOWL))
        }

        # BBFL2

        progress$set(value = 0.7, detail = "BBFL2")

        NameBBFL2 <- c("bbfl2_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameBBFL2))) {
          PotBBFL2 <- str_subset(ParsedFiles(), NameBBFL2)

          BBFL2(read_csv(PotBBFL2))
        }

        progress$set(value = 1, detail = "Done")

        # BioSonic

        NameBioSonic <- c("biosonic.*\\.csv")

        if (any(str_detect(ParsedFiles(), NameBioSonic))) {
          PotBioSonic <- str_subset(ParsedFiles(), NameBioSonic)

          BioSonic(read_csv(PotBioSonic))
        }

        # HBDevices

        NameHBDevices <- c("hb_devices_.*\\.csv")

        if (any(str_detect(ParsedFiles(), NameHBDevices))) {
          PotHBDevices <- str_subset(ParsedFiles(), NameHBDevices)

          HBDevices(read_csv(PotHBDevices))
        }
      }
    )

    # MainLog -----------------------------------------------------------------

    observeEvent(
      ignoreInit = FALSE,
      {
        c(
          SearProj()
        )
      },
      {
        NameMainLog <- "main_log_[:digit:]{8}_[:digit:]{6}\\.csv"
        ParsedDir <- file.path(SearProj()$ProjPath, "sear", "data", "parsed")

        # PotMainLog <- file.path(ParsedDir, paste0("main_log_",Sysdate_time,".csv"))

        if (any(str_detect(ParsedFiles(), NameMainLog))) {
          PotMainLog <- str_subset(ParsedFiles(), NameMainLog)

          OldMainLog <- read_csv(PotMainLog)

          MainLog(OldMainLog)
        }
      }
    )

    UpdateMainLog <- observeEvent(
      {
        c(
          Apla(),
          HBDevices()
        )
      },
      {
        validate(need(!is.null(Apla()) | !is.null(HBDevices()), "Need either Apla or HBDevices"))

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "MainLog check", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        OldMainLog <- MainLog()

        if (!is.null(Apla())) {
          PrimMainLog <- Apla()
        } else if (!is.null(HBDevices())) {
          PrimMainLog <- HBDevices()
        }

        # TODO Need to figure out how to compute on BioSonic Load
        # | !is.null(BioSonic$BioSonic())

        if (nrow(OldMainLog) == nrow(PrimMainLog)) {
          message("MainLog up to date")

          progress$set(value = 1, detail = " up to date")
        } else {
          message("Updating MainLog")

          progress$set(value = 0, message = "Updating MainLog: ")

          PrimBioSonic <- BioSonic()

          PrimHBDevices <- HBDevices()

          if (!is.null(HOCRtimeIndex())) {
            # Keep only one record by second (minimum necessary to compare against)
            HOCRtimeIndex <- unique(HOCRtimeIndex())

            progress$set(value = 0.1, detail = "HOCR synthesis")
            DataSyntHOCR <- data_synthesis(PrimMainLog$date_time, HOCRtimeIndex)
            message("HOCR synthesis done")
          } else {
            DataSyntHOCR <- NA
          }

          # TODO quick fix for SBE19() present but df is empty
          if (!is.null(SBE19())) {
            if (nrow(SBE19()) > 0) {
              SBE19timeIndex <- ymd_hms(unique(format(SBE19()$date_time, "%Y-%m-%d %H:%M:%S")))

              progress$set(value = 0.5, detail = "SBE19 synthesis")
              DataSyntSBE19 <- data_synthesis(PrimMainLog$date_time, SBE19timeIndex)
              message("SBE19 synthesis done")
            } else {
              DataSyntSBE19 <- NA
            }

          } else {
            DataSyntSBE19 <- NA
          }

          if (!is.null(SeaOWL())) {
            SeaOWLtimeIndex <- ymd_hms(unique(format(SeaOWL()$date_time, "%Y-%m-%d %H:%M:%S")))

            progress$set(value = 0.6, detail = "SeaOWL synthesis")
            DataSyntSeaOWL <- data_synthesis(PrimMainLog$date_time, SeaOWLtimeIndex)
            message("SeaOWL synthesis done")
          } else {
            DataSyntSeaOWL <- NA
          }

          if (!is.null(BBFL2())) {
            BBFL2timeIndex <- ymd_hms(unique(format(BBFL2()$date_time, "%Y-%m-%d %H:%M:%S")))

            progress$set(value = 0.7, detail = "BBFL2 synthesis")
            DataSyntBBFL2 <- data_synthesis(PrimMainLog$date_time, BBFL2timeIndex)
            message("BBFL2 synthesis done")
          } else {
            DataSyntBBFL2 <- NA
          }

          if (!is.null(PrimBioSonic$date_time)) {
            BioSonictimeIndex <- ymd_hms(unique(format(PrimBioSonic$date_time, "%Y-%m-%d %H:%M:%S")))

            progress$set(value = 0.8, detail = "BioSonic synthesis")
            DataSyntBioSonic <- data_synthesis(PrimMainLog$date_time, BioSonictimeIndex)
            message("BioSonic synthesis done")
          } else {
            DataSyntBioSonic <- NA
          }

          # if (!is.null(PrimHBDevices$date_time)) {
          #   HBDevicestimeIndex <- ymd_hms(unique(format(PrimHBDevices$date_time, "%Y-%m-%d %H:%M:%S")))
          #
          #   progress$set(value = 0.8, detail = "HBDevices synthesis")
          #   DataSyntHBDevices <- data_synthesis(PrimMainLog$date_time, HBDevicestimeIndex)
          #   message("HBDevices synthesis done")
          # } else {
          #   DataSyntHBDevices <- NA
          # }

          progress$set(value = 0.9, detail = "Saving")

          PrimMainLog <- PrimMainLog %>%
            mutate(
              id = seq_along(rownames(PrimMainLog)),
              HOCR = DataSyntHOCR,
              SBE19 = DataSyntSBE19,
              SeaOWL = DataSyntSeaOWL,
              BBFL2 = DataSyntBBFL2,
              BioSonic = DataSyntBioSonic,
            )

          MainLog(PrimMainLog)

          ParsedDir <- file.path(SearProj()$ProjPath, "sear", "data", "parsed")
          dir.create(ParsedDir, recursive = TRUE)

          Sysdate_time <- format(as.POSIXlt(Sys.time(), tz = "utc"), "%Y%m%d_%height_watercolumn%M%S")

          NameMainLog <- "main_log_[:digit:]{8}_[:digit:]{6}\\.csv"

          if (any(str_detect(ParsedFiles(), NameMainLog))) {
            PotMainLog <- str_subset(ParsedFiles(), NameMainLog)

            file.remove(PotMainLog)
          }

          PotMainLog <- file.path(ParsedDir, paste0("main_log_", Sysdate_time, ".csv"))

          write_csv(MainLog(), PotMainLog)

          progress$set(value = 0.1, detail = "Done")
        }
      }
    )

    output$SurveyDuration <- renderText({
      validate(need(MainLog(), message = "Need MainLog"))

      time <- MainLog()$date_time

      as.character(dseconds(length(time)))
    })

    # Module output -----------------------------------------------------------

    list(
      ParsedFiles = ParsedFiles,
      instrumentList = ToProcess$instrumentList,
      Apla = Apla,
      HOCR = HOCR,
      hocr_dark = hocr_dark,
      HOCRtimeIndex = HOCRtimeIndex,
      SBE19 = SBE19,
      SeaOWL = SeaOWL,
      BBFL2 = BBFL2,
      BioSonic = BioSonic,
      HBDevices = HBDevices
    )
  })
}

## To be copied in the UI
# mod_parse_data_ui("parse_data")

## To be copied in the server
# mod_parse_data_server("parse_data")
