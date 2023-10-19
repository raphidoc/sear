#' parse_mtelog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import readr shinyFeedback
mod_parse_mtelog_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #waiter::use_waiter(),
    uiOutput(outputId = ns("Load"))
  )
}

#' parse_mtelog Server Functions
#'
#' @noRd
mod_parse_mtelog_server <- function(id, SearTbl, CalData, ParsedFiles) {
  stopifnot(is.reactive(SearTbl))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    Apla <- reactiveVal()
    HOCR <- reactiveVal()
    HOCRDark <- reactiveVal()
    HOCRTimeIndex <- reactiveVal()
    SBE19 <- reactiveVal()
    SeaOWL <- reactiveVal()
    BBFL2 <- reactiveVal()

# Create parsed files on input --------------------------------------------

    output$Load <- renderUI({
      req(SearTbl())

      validate(need(CalData$CalHOCR(), message = "Need HOCR calibration data"))

      fluidRow(
        column(
          width = 6,
          fileInput(ns("Files"), "Choose MTE .txt and .bin Files", accept = c(".txt",".bin"), multiple = T)
        )
      )

    })

    Input <- reactive(input$Files)

    observeEvent(
      #ignoreInit = T,
      Input(),
      {

        # waiter <- waiter::Waiter$new()
        # waiter$show()
        # on.exit(waiter$hide())

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Parsing MTE: ", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # Copy files in raw dir

        progress$set(value = 0.1, detail = "Copy raw file")

        RawDir <- file.path(SearTbl()$ProjPath, "sear", "data", "raw")

        dir.create(RawDir, recursive = TRUE)

        Files <- input$Files %>%
          mutate(
            rawpath = file.path(RawDir, name)
          )

        # CAREFUL HERE ! should diplay a modal asking the user to confirm the overwrite operation
        file.copy(Files$datapath, Files$rawpath, overwrite = T)

        # Variables to create parsed files

        MTELog <- read_mtelog(str_subset(Files$rawpath, "\\.txt$"))

        InstList <- unique(MTELog$Instrument)

        DateTime <- str_extract(str_subset(Files$name, "\\.txt$"), "[:digit:]{8}_[:digit:]{6}")

        ParsedDir <- file.path(SearTbl()$ProjPath, "sear", "data", "parsed")

        dir.create(ParsedDir, recursive = TRUE)

        # Applanix

        progress$set(value = 0.2, detail = "Applanix")

        PotApla <- file.path(ParsedDir, paste0("apla_",DateTime,".csv"))

        if (any(str_detect(InstList, "APLA")) & !file.exists(PotApla)) {

          PrimApla <- read_apla(MTELog)

          write_csv(PrimApla, PotApla) # Should I use append = T to add data ?
        }

        # HOCR

        # Chech that all three HOCR are present
        # OCRList <- InstrumentList[str_detect(InstrumentList, "OCR")]
        #
        # if (any(OCRList %in% c("OCR1", "OCR2", "OCR3")) & any(!OCRList %in% c("OCR1", "OCR2", "OCR3"))) {
        #   MissingOCR <- c("OCR1", "OCR2", "OCR3")[which(!c("OCR1", "OCR2", "OCR3") %in% InstrumentList)]
        #
        #   warning("HOCR on port ", MissingOCR, "is missing from MainLog. Cannot process HOCR data.")
        #
        #   InstrumentList <- str_remove(InstrumentList, "OCR1|OCR2|OCR3")
        # }

        progress$set(value = 0.3, detail = "HOCR")

        PotHOCR <- file.path(ParsedDir, paste0("hocr_",DateTime,".rds"))
        PotHOCRDark <- file.path(ParsedDir, paste0("hocr_dark_",DateTime,".rds"))
        PotHOCRTimeIndex <- file.path(ParsedDir, paste0("hocr_time_index_",DateTime,".rds"))

        if (any(str_detect(InstList, "OCR"), na.rm = T) & !file.exists(PotHOCR)) {

          PrimHOCR <- read_hocr(str_subset(Files$rawpath, "\\.bin"))

          PrimHocrDarkRaw <- PrimHOCR[purrr::map_lgl(PrimHOCR, ~ str_detect(.$instrument, "HED|PLD"))]

          # Dont know the logger date format so quick fix with Apla date
          AplaDate <- unique(date(PrimApla$DateTime))

          progress$set(value = 0.4, message = "Calibrate HOCR dark")

          PrimHOCRDark <- cal_dark(PrimHocrDarkRaw, CalHOCR = CalData$CalHOCR(), AplaDate)

          progress$set(value = 0.5, message = "Creating HOCR time index")

          # Posixct object appear to be heavy, same length list of DateTime is heavier (25.8 MB) than the list of HOCR packets (22.2)
          # Computation time arround 2/3 minutes
          TimeIndex <- purrr::map(
            .x = PrimHOCR,
            ~ as.numeric(clock::date_time_parse(
              paste0(AplaDate, " ", hms::as_hms(.x$gpstime / 1000)),
              zone = "UTC"))
          )

          if (length(TimeIndex) != length(PrimHOCR)) {
            stop("HOCRTimeIndex not the same length as HOCR !")
          }

          PrimHOCRTimeIndex <- as.POSIXct(unlist(TimeIndex), tz = "UTC")

          progress$set(value = 0.6, message = "Writing HOCR data")

          write_rds(PrimHOCR, PotHOCR)

          write_rds(PrimHOCRDark, PotHOCRDark)

          write_rds(PrimHOCRTimeIndex, PotHOCRTimeIndex)

        }

        progress$set(message = "Parsing MTE: ", value = 0.6)

        # SBE19

        progress$set(value = 0.7, detail = "SBE19")

        PotSBE19 <- file.path(ParsedDir, paste0("sbe19_",DateTime,".csv"))

        if (any(str_detect(InstList, "CTD")) & !file.exists(PotSBE19)) {

          PrimSBE19 <- read_sbe19(MTELog)

          write_csv(PrimSBE19, PotSBE19)

        }

        # SeaOWL

        progress$set(value = 0.8, detail = "SeaOWL")

        PotSeaOWL <- file.path(ParsedDir, paste0("seaowl_",DateTime,".csv"))

        if (any(str_detect(InstList, "OWL")) & !file.exists(PotSeaOWL)) {

          PrimSeaOWL <- read_seaowl(MTELog)

          write_csv(PrimSeaOWL, PotSeaOWL)

        }

        # BBFL2

        progress$set(value = 0.9, detail = "BBFL2")

        PotBBFL2 <- file.path(ParsedDir, paste0("bbfl2_",DateTime,".csv"))

        if (any(str_detect(InstList, "ECO")) & !file.exists(PotBBFL2)) {

          PrimBBFL2 <- read_bbfl2(MTELog)

          write_csv(PrimBBFL2, PotBBFL2)

        }

        progress$set(value = 1, detail = "Done")

      }
    )

# Read parsed files on project load ---------------------------------------
# This should be located in the parse_data module and read data parsed to
# sear specification. The sear format specification must be defined before.

    # ParsedData is used to keep track of the loaded metadata (Apla) against MainLog
    ParsedData <- eventReactive(
      #ignoreInit = TRUE,
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
              Instrument = .x$Instrument,
              SN = .x$SN,
              AproxData = purrr::map(
                .x = ..1$AproxData,
                ~ pivot_longer(
                  .x,
                  cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
                  values_to = "Channels",
                  names_to = c("Type", "Wavelength"),
                  names_sep = "_",
                  # names_prefix = "[[:alpha:]]{2}_",
                  names_transform = list(Wavelength = as.numeric)
                )
              )
            )
          )

          test2 <- purrr::map(.x = test, ~ unnest(.x, cols = c(AproxData)))

          test3 <- purrr::map_df(.x = test2, ~ .x) %>%
            group_by(Instrument, SN) %>%
            nest(AproxData = !matches("Instrument|SN")) %>%
            mutate(
              AproxData = purrr::map(
                AproxData,
                ~ pivot_wider(
                  .x,
                  names_from = all_of(c("Type", "Wavelength")),
                  names_sep = "_",
                  values_from = Channels
                )
              )
              )

          HOCRDark(test3)

        }

        progress$set(value = 0.4, detail = "HOCR time index")

        NameHOCRTimeIndex <- c("hocr_time_index_[:digit:]{8}_[:digit:]{6}\\.rds")

        if (any(str_detect(ParsedFiles(), NameHOCRTimeIndex))) {

          PotHOCRTimeIndex <- str_subset(ParsedFiles(), NameHOCRTimeIndex)

          # unlist convert posixct DateTime representation back to number of seconds
          # temp <- as.POSIXct(
          #   unlist(
          #     purrr::map(.x = PotHOCRTimeIndex, ~ read_rds(.x)),
          #     recursive = T),
          #   tz = "UTC")

          temp <- unlist(
            purrr::map(.x = PotHOCRTimeIndex, ~ read_rds(.x)),
            recursive = T)

          HOCRTimeIndex(temp)

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

        return(Apla())

      }
    )


    # Module output -----------------------------------------------------------
    list(
      ParsedData = ParsedData,
      Apla = Apla,
      HOCR = HOCR,
      HOCRDark = HOCRDark,
      HOCRTimeIndex = HOCRTimeIndex,
      BBFL2 = BBFL2,
      SeaOWL = SeaOWL,
      SBE19 = SBE19,
      Input = Input
    )
  })
}

## To be copied in the UI
# mod_parse_mtelog_ui("parse_mtelog")

## To be copied in the server
# mod_parse_mtelog_server("parse_mtelog")
