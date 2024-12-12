#' parse_satview UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_satview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("Load"))
  )
}

#' parse_satview Server Functions
#'
#' @noRd
mod_parse_satview_server <- function(id, SearProj, cal_data, ParsedFiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Load <- renderUI({
      req(SearProj())

      validate(need(cal_data$hocr_cal(), message = "Need HOCR calibration data"))

      fluidRow(
        column(
          width = 6,
          fileInput(ns("Files"), "Choose SatView .raw files", accept = c(".raw"), multiple = T)
        )
      )
    })

    Input <- reactive(input$Files)

    observeEvent(
      # ignoreInit = T,
      Input(),
      {
        # waiter <- waiter::Waiter$new()
        # waiter$show()
        # on.exit(waiter$hide())

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Parsing SatView: ", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # Copy files in raw dir

        progress$set(value = 0.1, detail = "Copy raw file")

        RawDir <- file.path(SearProj()$ProjPath, "sear", "data", "raw")

        dir.create(RawDir, recursive = TRUE)

        Files <- input$Files %>%
          mutate(
            rawpath = file.path(RawDir, name)
          )

        # CAREFUL HERE ! should display a modal asking the user to confirm the overwrite operation
        file.copy(Files$datapath, Files$rawpath, overwrite = T)

        # Variables to create parsed files

        # MTELog <- read_mtelog(str_subset(Files$rawpath, "\\.txt$"))

        InstList <- "OCR" # unique(MTELog$instrument)

        date_time <- str_extract(str_subset(Files$name, "\\.raw$"), "[:digit:]{4}-[:digit:]{3}-[:digit:]{6}")

        Year <- str_extract(date_time, "^[:digit:]{4}(?=-)")

        DOY <- str_extract(date_time, "(?<=-)[:digit:]{3}(?=-)")

        date <- as.date(as.numeric(DOY) - 1, origin = as.date(paste0(Year, "-01-01")))

        time <- str_extract(date_time, "(?<=-)[:digit:]{6}$")

        date_time <- lubridate::ymd_hms(paste(date, time))
        date_time <- format(date_time, "%Y%m%d_%height_watercolumn%M%S")

        ParsedDir <- file.path(SearProj()$ProjPath, "sear", "data", "parsed")

        dir.create(ParsedDir, recursive = TRUE)

        # HOCR

        # Chech that all three HOCR are present
        # OCRList <- instrumentList[str_detect(instrumentList, "OCR")]
        #
        # if (any(OCRList %in% c("OCR1", "OCR2", "OCR3")) & any(!OCRList %in% c("OCR1", "OCR2", "OCR3"))) {
        #   MissingOCR <- c("OCR1", "OCR2", "OCR3")[which(!c("OCR1", "OCR2", "OCR3") %in% instrumentList)]
        #
        #   warning("HOCR on port ", MissingOCR, "is missing from MainLog. Cannot process HOCR data.")
        #
        #   instrumentList <- str_remove(instrumentList, "OCR1|OCR2|OCR3")
        # }

        progress$set(value = 0.1, detail = "HOCR")

        PotHOCR <- file.path(ParsedDir, paste0("hocr_", date_time, ".rds"))
        PotHOCRDark <- file.path(ParsedDir, paste0("hocr_dark_", date_time, ".rds"))
        PotHOCRtimeIndex <- file.path(ParsedDir, paste0("hocr_time_index_", date_time, ".rds"))

        if (any(str_detect(InstList, "OCR"), na.rm = T) & !file.exists(PotHOCR)) {
          progress$set(value = 0.2, message = "Reading SatView .raw")

          PrimHOCR <- read_satview_hocr(str_subset(Files$rawpath, "\\.raw"))

          PrimHocrDarkRaw <- PrimHOCR[purrr::map_lgl(PrimHOCR, ~ str_detect(.$instrument, "PED|HED|PLD|HLD"))]

          progress$set(value = 0.4, message = "Calibrate HOCR dark")

          PrimHOCRDark <- cal_dark(PrimHocrDarkRaw, hocr_cal = cal_data$hocr_cal(), date = "data")

          progress$set(value = 0.6, message = "Creating HOCR time index")

          # Posixct object appear to be heavy, same length list of date_time is heavier (25.8 MB) than the list of HOCR packets (22.2)
          # Computation time arround 2/3 minutes
          timeIndex <- purrr::map(
            .x = PrimHOCR,
            function(.x) {
              Year <- str_extract(.x$date, "^[:digit:]{4}")

              DOY <- str_extract(.x$date, "[:digit:]{3}$")

              date <- as.date(as.numeric(DOY) - 1, origin = as.date(paste0(Year, "-01-01")))

              time <- substring(.x$time, c(1, 3, 5, 7), c(2, 4, 6, 9))

              HMS <- str_c(time[1:3], collapse = ":")
              HMSmmm <- str_c(HMS, time[4], sep = ".")

              return(as.numeric(ymd_hms(paste(date, HMSmmm))))
            }
          )

          if (length(timeIndex) != length(PrimHOCR)) {
            stop("HOCRtimeIndex not the same length as HOCR !")
          }

          PrimHOCRtimeIndex <- as.POSIXct(unlist(timeIndex), tz = "utc")

          progress$set(value = 0.8, message = "Writing HOCR data")

          write_rds(PrimHOCR, PotHOCR)

          write_rds(PrimHOCRDark, PotHOCRDark)

          write_rds(PrimHOCRtimeIndex, PotHOCRtimeIndex)
        }

        progress$set(value = 1, message = "DONE")
      }
    )

    # Module output -----------------------------------------------------------

    return(Input)
  })
}

## To be copied in the UI
# mod_parse_satview_ui("parse_satview")

## To be copied in the server
# mod_parse_satview_server("parse_satview")
