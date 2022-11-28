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
    waiter::use_waiter(),
    mod_select_instrument_ui(ns("select_instrument")),
    uiOutput(outputId = ns("TabPanel"))
  )
}

#' parse_data Server Functions
#'
#' @noRd
mod_parse_data_server <- function(id, SearTbl, CalData, MainLog){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$TabPanel <- renderUI({
      req(SearTbl())

      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
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
          SearTbl(),
          MTELog$Input(),
          BioSonic$Input()
          )
      },
      {

        ParsedDir <- file.path(SearTbl()$ProjPath, ".sear", "data", "parsed")

        if (dir.exists(ParsedDir)) {

          list.files(ParsedDir, full.names = TRUE)

        } else {
          FALSE
        }

      }
    )

    ToProcess <- mod_select_instrument_server("select_instrument", ParsedFiles)
    MTELog <- mod_parse_mtelog_server("parse_mtelog", SearTbl, CalData, ParsedFiles)
    BioSonic <- mod_parse_biosonic_server("parse_biosonic", SearTbl, ParsedFiles)


# MainLog -----------------------------------------------------------------

    observeEvent(
      ignoreInit = FALSE,
      {
        c(
          SearTbl()
          )
      },
      {

        NameMainLog <- "main_log_[:digit:]{8}_[:digit:]{6}\\.csv"
        ParsedDir <- file.path(SearTbl()$ProjPath, ".sear", "data", "parsed")

        #PotMainLog <- file.path(ParsedDir, paste0("main_log_",SysDateTime,".csv"))

        if (any(str_detect(ParsedFiles(), NameMainLog))) {

          PotMainLog <- str_subset(ParsedFiles(), NameMainLog)

          PrimMainLog <- read_csv(PotMainLog)

          MainLog(PrimMainLog)

        }

      }
    )

    UpdateMainLog <- observeEvent(
      {
        MTELog$ParsedData()
      },
      {

        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())

        browser()

        OldMainLog <- MainLog()

        PrimMainLog <- MTELog$Apla() %>%
          rename(Lon = Lon_DD, Lat = Lat_DD)

        if (nrow(OldMainLog) == nrow(PrimMainLog) | !is.null(BioSonic$BioSonic())) {

          message("MainLog up to date")

        } else {

          PrimBioSonic <- BioSonic$BioSonic()

          # HOCR often record 2/3 bin per second.
          # Should we reduce the time index by second to optimize computation time ?
          # Also this time index represents all three hocr, could start by keeping only one

          DataSyntHOCR <- data_synthesis(PrimMainLog$DateTime, MTELog$HOCRTimeIndex())

          DataSyntSBE19 <- data_synthesis(PrimMainLog$DateTime, MTELog$SBE19()$DateTime)

          DataSyntSeaOWL <- data_synthesis(PrimMainLog$DateTime, MTELog$SeaOWL()$DateTime)

          DataSyntBBFL2 <- data_synthesis(PrimMainLog$DateTime, MTELog$BBFL2()$DateTime)

          DataSyntBioSonic <- data_synthesis(PrimMainLog$DateTime, PrimBioSonic$DateTime)

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

          ParsedDir <- file.path(SearTbl()$ProjPath, ".sear", "data", "parsed")
          dir.create(ParsedDir, recursive = TRUE)

          SysDateTime <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y%m%d_%H%M%S")

          NameMainLog <- "main_log_[:digit:]{8}_[:digit:]{6}\\.csv"

          if (any(str_detect(ParsedFiles(), NameMainLog))) {

            PotMainLog <- str_subset(ParsedFiles(), NameMainLog)

            file.remove(PotMainLog)

          }

          PotMainLog <- file.path(ParsedDir, paste0("main_log_",SysDateTime,".csv"))

          PrimMainLog <- write_csv(MainLog(), PotMainLog)


        }

      }
    )

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
