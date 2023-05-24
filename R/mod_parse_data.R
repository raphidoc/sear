#' parse_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("TabPanel")),
    textOutput(ns("SurveyDuration"))
  )
}

#' parse_data Server Functions
#'
#' @noRd
mod_parse_data_server <- function(id, SearProj, CalData, MainLog){
  moduleServer( id, function(input, output, session){
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
        )
      )
    })

    ParsedFiles <- eventReactive(
      {
        c(
          SearProj(),
          MTELog$Input(),
          BioSonic$Input()
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

    ToProcess <- mod_select_instrument_server("select_instrument", ParsedFiles)
    MTELog <- mod_parse_mtelog_server("parse_mtelog", SearProj, CalData, ParsedFiles)
    BioSonic <- mod_parse_biosonic_server("parse_biosonic", SearProj, ParsedFiles)


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

        #PotMainLog <- file.path(ParsedDir, paste0("main_log_",SysDateTime,".csv"))

        if (any(str_detect(ParsedFiles(), NameMainLog))) {

          PotMainLog <- str_subset(ParsedFiles(), NameMainLog)

          OldMainLog <- read_csv(PotMainLog)

          MainLog(OldMainLog)

        }

      }
    )

    UpdateMainLog <- observeEvent(
      {
        MTELog$ParsedData()
      },
      {

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "MainLog check", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        OldMainLog <- MainLog()

        PrimMainLog <- MTELog$Apla() %>%
          rename(Lon = Lon_DD, Lat = Lat_DD)

        # TODO Need to figure out how to compute on BioSonic Load
        # | !is.null(BioSonic$BioSonic())

        if (nrow(OldMainLog) == nrow(PrimMainLog)) {

          message("MainLog up to date")

          progress$set(value = 1, detail = " up to date")

        } else {

          message("Updating MainLog")

          progress$set(value = 0, message = "Updating MainLog: ")

          PrimBioSonic <- BioSonic$BioSonic()

          if (!is.null(MTELog$HOCRTimeIndex())) {
            # Keep only one record by second (minimum necessary to compare against)
            HOCRTimeIndex <- unique(MTELog$HOCRTimeIndex())

            progress$set(value = 0.1, detail = "HOCR synthesis")
            DataSyntHOCR <- data_synthesis(PrimMainLog$DateTime, HOCRTimeIndex)
            message("HOCR synthesis done")

          } else {
            DataSyntHOCR <- NA
          }

          SBE19TimeIndex <- ymd_hms(unique(format(MTELog$SBE19()$DateTime, "%Y-%m-%d %H:%M:%S")))

          progress$set(value = 0.5, detail = "SBE19 synthesis")
          DataSyntSBE19 <- data_synthesis(PrimMainLog$DateTime, SBE19TimeIndex)
          message("SBE19 synthesis done")


          SeaOWLTimeIndex <- ymd_hms(unique(format(MTELog$SeaOWL()$DateTime, "%Y-%m-%d %H:%M:%S")))

          progress$set(value = 0.6, detail = "SeaOWL synthesis")
          DataSyntSeaOWL <- data_synthesis(PrimMainLog$DateTime, SeaOWLTimeIndex)
          message("SeaOWL synthesis done")


          BBFL2TimeIndex <- ymd_hms(unique(format(MTELog$BBFL2()$DateTime, "%Y-%m-%d %H:%M:%S")))

          progress$set(value = 0.7, detail = "BBFL2 synthesis")
          DataSyntBBFL2 <- data_synthesis(PrimMainLog$DateTime, BBFL2TimeIndex)
          message("BBFL2 synthesis done")

          if (!is.null(PrimBioSonic$DateTime)) {
            BioSonicTimeIndex <- ymd_hms(unique(format(PrimBioSonic$DateTime, "%Y-%m-%d %H:%M:%S")))

            progress$set(value = 0.8, detail = "BioSonic synthesis")
            DataSyntBioSonic <- data_synthesis(PrimMainLog$DateTime, BioSonicTimeIndex)
            message("BioSonic synthesis done")
          } else {
            DataSyntBioSonic <- NA
          }

          progress$set(value = 0.9, detail = "Saving")

          PrimMainLog <- PrimMainLog %>%
            mutate(
              ID = seq_along(rownames(PrimMainLog)),
              HOCR = DataSyntHOCR,
              SBE19 = DataSyntSBE19,
              SeaOWL = DataSyntSeaOWL,
              BBFL2 = DataSyntBBFL2,
              BioSonic = DataSyntBioSonic
            )

          MainLog(PrimMainLog)

          ParsedDir <- file.path(SearProj()$ProjPath, "sear", "data", "parsed")
          dir.create(ParsedDir, recursive = TRUE)

          SysDateTime <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y%m%d_%H%M%S")

          NameMainLog <- "main_log_[:digit:]{8}_[:digit:]{6}\\.csv"

          if (any(str_detect(ParsedFiles(), NameMainLog))) {

            PotMainLog <- str_subset(ParsedFiles(), NameMainLog)

            file.remove(PotMainLog)

          }

          PotMainLog <- file.path(ParsedDir, paste0("main_log_",SysDateTime,".csv"))

          PrimMainLog <- write_csv(MainLog(), PotMainLog)

          progress$set(value = 0.1, detail = "Done")

        }

      }
    )

    output$SurveyDuration <- renderText({

      validate(need(MainLog(), message = "Need MainLog"))

      Time <- MainLog()$DateTime

      as.character(dseconds(length(Time)))

    })

# Module output -----------------------------------------------------------

    append(
      list(
        ParsedFiles = ParsedFiles
      ),
      c(
        ToProcess,
        MTELog,
        BioSonic
      )
    )

  })
}

## To be copied in the UI
# mod_parse_data_ui("parse_data")

## To be copied in the server
# mod_parse_data_server("parse_data")
