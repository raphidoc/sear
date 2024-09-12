#' process_L1L2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1b_process_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("ProcessL1b"), "Process L1b")
  )
}

#' process_L1L2 Server Functions
#'
#' @noRd
mod_L1b_process_server <- function(id, L1a, L1aSelect, CalData, Obs, MainLog, Settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(
      list(
        input$ProcessL1b
      ),
      label = "processL1b",
      ignoreInit = T,
      {
        if (is.null(L1a$InstrumentList())) {
          showModal(modalDialog(
            title = "No instrument selected",
            "Please select at least one instrument to process"
          ))
          invalidateLater(1)
        }

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Processing L1b: ", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # Create the discrete time interval and select the associated points
        TimeInt <- interval(
          min(L1aSelect$MainLog()$DateTime, na.rm = T),
          max(L1aSelect$MainLog()$DateTime, na.rm = T)
        )

        Select <- MainLog()[(MainLog()$DateTime %within% TimeInt), ]

        # Create metadata for the selected L1a point

        Obs$MetadataL1b <- gen_metadataL1b(Select = Select)

        Obs$MetadataL2 <- gen_metadataL2(Select = Select)

        # Empty L1b and L2 on new processing to avoid confusion
        Obs$HOCR$L1b <- tibble()
        Obs$SBE19$L1b <- tibble()
        Obs$SeaOWL$L1b <- tibble()
        Obs$BBFL2$L1b <- tibble()
        Obs$BioSonic$L1b <- tibble()

        Obs$HOCR$L2 <- tibble()
        Obs$SBE19$L2 <- tibble()
        Obs$SeaOWL$L2 <- tibble()
        Obs$BBFL2$L2 <- tibble()
        Obs$BioSonic$L2 <- tibble()

        # SBE19 L1b ---------------------------------------------------------------

        if (any(str_detect(L1a$InstrumentList(), "SBE19"))) {
          progress$set(message = "Processing L1b: ", value = progress$getValue())

          progress$set(value = 0.3, detail = "SBE19")

          Lon <- mean(Select$Lon)
          Lat <- mean(Select$Lat)

          SBE19 <- L1a$SBE19() %>% filter(DateTime %within% TimeInt)

          if (nrow(SBE19) == 0) {
            warning(
              paste0("SBE19 data not found at time interval: ", TimeInt)
            )
          } else if (is.null(CalData$CalSBE19()) | is.null(CalData$CalSBE18()) | is.null(CalData$CalSBE43())) {
            warning(
              "SBE19 | SBE18 | SBE43 calibration data not loaded"
            )
          } else {
            SBE19 <- cal_sbe19(SBE19, Lon, Lat) %>%
              mutate(
                Oxygen = cal_sbe43( # Oxygen in ml/l multiply by 1.42903 to get mg/l
                  Volt = Volt0,
                  Tcelsius = Temperature,
                  Pressure = Pressure,
                  OxSol = OxSol,
                  CalData = CalData$CalSBE43()
                )
              ) %>%
              mutate(
                pH = cal_sbe18(
                  Volt = Volt2,
                  Tcelsius = Temperature,
                  CalData = CalData$CalSBE18()
                )
              ) %>%
              select(
                DateTime,
                Temperature,
                Conductivity,
                Pressure,
                SP,
                SA,
                CT,
                O2Sol,
                OxSol,
                Oxygen,
                pH
              ) %>%
              mutate(
                ID = seq_along(rownames(SBE19)),
                QC = "1"
              )

            Obs$SBE19$L1b <- SBE19 %>%
              select(!any_of(c("Conductivity", "CT", "O2Sol"))) %>%
              pivot_longer(
                cols = any_of(c("Temperature", "Pressure", "SP", "SA", "OxSol", "Oxygen", "pH")),
                names_to = "Parameter",
                values_to = "Value"
              ) %>%
              group_by(Parameter) %>%
              nest(Data = !matches("Parameter"))
          }
        }


        # HOCR L1b ----------------------------------------------------------------

        if (any(str_detect(L1a$InstrumentList(), "HOCR"))) {
          # Create a callback function to update progress.
          # Each time this is called:
          # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
          #   distance. If non-NULL, it will set the progress to that value.
          # - It also accepts optional detail text.
          UpdateProgress <- function(value = NULL, message = NULL, detail = NULL) {
            if (is.null(value)) {
              value <- progress$getValue()
              value <- value + (progress$getMax() - value) / 5
            }
            progress$set(value = value, message = message, detail = detail)
          }

          progress$set(value = 0.1, detail = "HOCR")

          FiltRawHOCR <- filter_hocr(L1a$HOCR(), L1a$HOCRTimeIndex(), TimeInt)

          if (length(FiltRawHOCR) == 0) {
            warning(
              paste0("HOCR data not found at time interval: ", TimeInt)
            )
          } else if (is.null(CalData$CalHOCR())) {
            warning(
              "HOCR calibration data not loaded"
            )
          } else {
            # Select nearest dark data
            ObsTime <- int_end(TimeInt / 2)

            HOCRDark <- L1a$HOCRDark() %>%
              mutate(DarkCalData = purrr::map(CalData, ~ .x[which.min(abs(ymd_hms(.x$DateTime) - ObsTime)), ])) %>%
              ungroup() %>%
              select(SN, DarkCalData)

            WaveSeq <- seq(
              Settings$HOCR$WaveMin(),
              Settings$HOCR$WaveMax(),
              Settings$HOCR$WaveStep()
            )

            Obs$HOCR$L1b <- spsComps::shinyCatch(
              cal_hocr(
                RawHOCR = FiltRawHOCR,
                CalHOCR = CalData$CalHOCR(),
                HOCRDark = HOCRDark,
                MetadataL1b = Obs$MetadataL1b,
                UpdateProgress,
                WaveSeq = WaveSeq
              ),
              shiny = T,
              trace_back = TRUE
            )
          }
        }

        # SeaOWL L1b --------------------------------------------------------------

        if (any(str_detect(L1a$InstrumentList(), "SeaOWL"))) {
          progress$set(value = 0.4, detail = "SeaOWL")

          SeaOWL <- L1a$SeaOWL() %>% filter(DateTime %within% TimeInt)

          if (nrow(SeaOWL) == 0) {
            warning(
              paste0("SeaOWL data not found at time interval: ", TimeInt)
            )
          } else if (is.null(CalData$CalSeaOWL())) {
            warning(
              "SeaOWL calibration data not loaded"
            )
          } else {
            SeaOWLL1b <- cal_seaowl(SeaOWL, CalData$CalSeaOWL()) %>%
              mutate(
                ID = seq_along(rownames(SeaOWL)),
                QC = "1"
              )

            Obs$SeaOWL$L1b <- SeaOWLL1b %>%
              select(!any_of(c("SN"))) %>%
              pivot_longer(
                cols = any_of(c("VSF_700", "Chl", "FDOM")),
                names_to = "Parameter",
                values_to = "Value"
              ) %>%
              group_by(Parameter) %>%
              nest(Data = !matches("Parameter"))
          }
        }

        # BBFL2 L1b ---------------------------------------------------------------

        if (any(str_detect(L1a$InstrumentList(), "BBFL2"))) {
          progress$set(value = 0.5, detail = "BBFL2")

          BBFL2 <- L1a$BBFL2() %>% filter(DateTime %within% TimeInt)

          if (nrow(BBFL2) == 0) {
            warning(
              paste0("BBFL2 data not found at time interval: ", TimeInt)
            )
          } else if (is.null(CalData$CalBBFL2())) {
            warning(
              "BBFL2 calibration data not loaded"
            )
          } else {
            BBFL2L1b <- cal_bbfl2(BBFL2, CalData$CalBBFL2()) %>%
              mutate(
                ID = seq_along(rownames(BBFL2)),
                QC = "1"
              )

            Obs$BBFL2$L1b <- BBFL2L1b %>%
              pivot_longer(
                cols = any_of(c("NTU", "PE", "PC")),
                names_to = "Parameter",
                values_to = "Value"
              ) %>%
              group_by(Parameter) %>%
              nest(Data = !matches("Parameter"))
          }
        }

        # BioSonic L1b ---------------------------------------------------------------

        if (any(str_detect(L1a$InstrumentList(), "BioSonic"))) {
          progress$set(value = 0.6, detail = "BioSonic")

          BioSonicL1b <- L1a$BioSonic() %>% filter(DateTime %within% TimeInt)

          if (nrow(BioSonicL1b) == 0) {
            warning(
              paste0("BioSonic data not found at time interval: ", TimeInt)
            )
          } else {
            Obs$BioSonic$L1b <- BioSonicL1b %>%
              rename(Lon = Longitude_deg, Lat = Latitude_deg) %>%
              select(
                Lon,
                Lat,
                DateTime,
                Altitude_mReMsl,
                BottomElevation_m,
                PlantHeight_m,
                PercentCoverage
              )
            # mutate(
            #   ID = seq_along(rownames(BioSonicL1b)),
            #   QC = "1"
            # )
          }
        }


        # HydroBall L1b -----------------------------------------------------------

        if (any(str_detect(L1a$InstrumentList(), "HydroBall"))) {
          progress$set(value = 0.6, detail = "HydroBall")

          HydroBallL1b <- L1a$HBDevices() %>% filter(DateTime %within% TimeInt)

          if (nrow(HydroBallL1b) == 0) {
            warning(
              paste0("HydroBall data not found at time interval: ", TimeInt)
            )
          } else {
            Obs$HydroBall$L1b <- HydroBallL1b %>%
              rename(H = DBT_meter) %>%
              mutate(
                H = if_else(H == 0, NA, H),
                H = -H
              ) %>%
              select(
                Lon,
                Lat,
                DateTime,
                Altitude,
                H
              )
          }
        }

        progress$set(value = 1, detail = "Done")
      }
    )

    # Module output -----------------------------------------------------------
    list(
      # SelMainLog = L1aSelect$MainLog,
      Map = L1aSelect$Map,
      ProcessL1b = reactive(input$ProcessL1b)
    )
  })
}

## To be copied in the UI
# mod_L1b_process_ui("L1b_process")

## To be copied in the server
# mod_L1b_process_server("L1b_process")
