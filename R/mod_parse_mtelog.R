#' parse_mtelog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import readr
mod_parse_mtelog_ui <- function(id) {
  ns <- NS(id)
  tagList(
    waiter::use_waiter(),
    uiOutput(outputId = ns("Load"))
  )
}

#' parse_mtelog Server Functions
#'
#' @noRd
mod_parse_mtelog_server <- function(id, SearTbl, CalData, Apla) {
  stopifnot(is.reactive(SearTbl))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    HOCR <- reactiveVal()
    HOCRDark <- reactiveVal()
    HOCRTimeIndex <- reactiveVal()
    SBE19 <- reactiveVal()
    SeaOWL <- reactiveVal()
    BBFL2 <- reactiveVal()

# Create parsed files on input --------------------------------------------

    output$Load <- renderUI({
      req(SearTbl())

      fileInput(ns("Files"), "Choose MTE txt and bin Files", accept = c(".txt",".bin"), multiple = T)

    })


    MTELog <- reactive(
      label = "MTELog",
      {
      #req(DataFiles())

      read_mtelog(DataFiles()$txt)
    })

    observeEvent(
      input$Files,
      {
        browser()

        # Copy files in raw dir

        RawDir <- file.path(SearTbl()$ProjPath, ".sear", "data", "raw")

        dir.create(RawDir, recursive = TRUE)

        Files <- input$Files %>%
          mutate(
            rawpath = file.path(RawDir, name)
          )

        file.copy(Files$datapath, Files$rawpath)

        # Variables to create parsed files

        MTELog <- read_mtelog(str_subset(Files$rawpath, "\\.txt$"))

        InstList <- unique(MTELog$Instrument)

        DateTime <- str_extract(str_subset(Files$name, "\\.txt$"), "[:digit:]{8}_[:digit:]{6}")

        ParsedDir <- file.path(SearTbl()$ProjPath, ".sear", "data", "parsed")

        dir.create(ParsedDir, recursive = TRUE)

        # Applanix

        if (any(str_detect(InstList, "APLA"))) {

          Apla(read_apla(MTELog))

          PotApla <- file.path(ParsedDir, paste0("apla_",DateTime,".csv"))

          write_csv(Apla(), PotApla) # Should I use append = T to add data ?
        }

        # HOCR

        if (any(str_detect(InstList, "OCR"))) {

          HOCR(read_hocr(str_subset(Files$rawpath, "\\.bin")))

          HocrDarkRaw <- HOCR()[purrr::map_lgl(HOCR(), ~ str_detect(.$instrument, "HED|PLD"))]

          # Dont know the logger date format so quick fix with Apla date
          AplaDate <- unique(date(Apla()$DateTime))

          HOCRDark(cal_dark(HocrDarkRaw, CalHOCR = CalData$CalHOCR(), AplaDate))

          # Posixct object appear to be heavy, same length list of DateTime is heavier (25.8 MB) than the list of HOCR packets (22.2)
          # Computation time arround 2/3 minutes
          TimeIndex <- purrr::map(
            .x = HOCR(),
            ~ clock::date_time_parse(
              paste0(AplaDate, " ", hms::as_hms(.x$gpstime / 1000)),
              zone = "UTC")
          )

          HOCRTimeIndex(TimeIndex)


          PotHOCR <- file.path(ParsedDir, paste0("hocr_",DateTime,".rds"))
          PotHOCRDark <- file.path(ParsedDir, paste0("hocr_dark_",DateTime,".rds"))
          PotHOCRTimeIndex <- file.path(ParsedDir, paste0("hocr_time_index_",DateTime,".rds"))

          write_rds(HOCR(), PotHOCR)

          write_rds(HOCRDark(), PotHOCRDark)

          write_rds(HOCRTimeIndex(), PotHOCRTimeIndex)

        }

        # SBE19

        if (any(str_detect(InstList, "CTD"))) {

          SBE19(read_sbe19(MTELog))

          PotSBE19 <- file.path(ParsedDir, paste0("sbe19_",DateTime,".csv"))

          write_csv(SBE19(), PotSBE19)

        }

        # SeaOWL

        if (any(str_detect(InstList, "OWL"))) {

          SeaOWL(read_seaowl(MTELog))

          PotSeaOWL <- file.path(ParsedDir, paste0("seaowl_",DateTime,".csv"))

          write_csv(SeaOWL(), PotSeaOWL)

        }

        # BBFL2

        if (any(str_detect(InstList, "ECO"))) {

          BBFL2(read_bbfl2(MTELog))

          PotBBFL2 <- file.path(ParsedDir, paste0("bbfl2_",DateTime,".csv"))

          write_csv(BBFL2(), PotBBFL2)

        }

      }
    )

# Read parsed files on project load ---------------------------------------

    ParsedFiles <- reactive({

      req(SearTbl())

      ParsedDir <- file.path(SearTbl()$ProjPath, ".sear", "data", "parsed")

      if (dir.exists(ParsedDir)) {

        list.files(ParsedDir, full.names = TRUE)

      } else {
        FALSE
      }

    })

    observeEvent(
      SearTbl(),
      {
        browser()

        # Applanix

        NameApla <- c("apla_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameApla))) {

          PotApla <- str_subset(ParsedFiles(), NameApla)

          Apla(read_csv(PotApla))

        }

        # HOCR

        NameHOCR <- c("hocr_[:digit:]{8}_[:digit:]{6}\\.rds")

        if (any(str_detect(ParsedFiles(), NameHOCR))) {

          PotHOCR <- str_subset(ParsedFiles(), NameHOCR)

          HOCR(read_rds(PotHOCR))

        }

        NameHOCRDark <- c("hocr_dark_[:digit:]{8}_[:digit:]{6}\\.rds")

        if (any(str_detect(ParsedFiles(), NameHOCRDark))) {

          PotHOCRDark <- str_subset(ParsedFiles(), NameHOCRDark)

          HOCRDark(read_rds(PotHOCRDark))

        }

        NameHOCRTimeIndex <- c("hocr_time_index_[:digit:]{8}_[:digit:]{6}\\.rds")

        if (any(str_detect(ParsedFiles(), NameHOCRTimeIndex))) {

          PotHOCRTimeIndex <- str_subset(ParsedFiles(), NameHOCRTimeIndex)

          HOCRTimeIndex(read_rds(PotHOCRTimeIndex))

        }

        # SBE19

        NameSBE19 <- c("sbe19_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameSBE19))) {

          PotSBE19 <- str_subset(ParsedFiles(), NameSBE19)

          SBE19(read_csv(PotSBE19))

        }

        # SeaOWL

        NameSeaOWL <- c("seaowl_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameSeaOWL))) {

          PotSeaOWL <- str_subset(ParsedFiles(), NameSeaOWL)

          SeaOWL(read_csv(PotSeaOWL))

        }

        # BBFL2

        NameBBFL2 <- c("bbfl2_[:digit:]{8}_[:digit:]{6}\\.csv")

        if (any(str_detect(ParsedFiles(), NameBBFL2))) {

          PotBBFL2 <- str_subset(ParsedFiles(), NameBBFL2)

          BBFL2(read_csv(PotBBFL2))

        }

      }
    )


    # Module output -----------------------------------------------------------
    list(
      MTELog = MTELog,
      Apla = Apla,
      HOCR = HOCR,
      HOCRDark = HOCRDark,
      HOCRTimeIndex = HOCRTimeIndex,
      BBFL2 = BBFL2,
      SeaOWL = SeaOWL,
      SBE19 = SBE19
    )
  })
}

## To be copied in the UI
# mod_parse_mtelog_ui("parse_mtelog")

## To be copied in the server
# mod_parse_mtelog_server("parse_mtelog")
