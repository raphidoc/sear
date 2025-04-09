#' process_L1L2 UI Function
#'
#' @description a shiny Module.
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
mod_L1b_process_server <- function(id, L1a, L1aSelect, cal_data, Obs, MainLog, Settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(
      list(
        input$ProcessL1b
      ),
      label = "processL1b",
      ignoreInit = T,
      {
        if (is.null(L1a$instrumentList())) {
          showModal(modalDialog(
            title = "No instrument selected",
            "Please select at least one instrument to process"
          ))
          invalidatelater(1)
        }

        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Processing L1b: ", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # Create the discrete time interval and select the associated points
        timeInt <- interval(
          min(L1aSelect$MainLog()$date_time, na.rm = T),
          max(L1aSelect$MainLog()$date_time, na.rm = T)
        )

        Select <- MainLog()[(MainLog()$date_time %within% timeInt), ]

        # Create metadata for the selected L1a point

        Obs$metadata_l1b <- gen_metadataL1b(metadata = Select, ensemble = 0)

        Obs$metadata_l2 <- gen_metadataL2(metadata = Select, ensemble = 0)

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

        if (any(str_detect(L1a$instrumentList(), "SBE19"))) {
          progress$set(message = "Processing L1b: ", value = progress$getValue())

          progress$set(value = 0.3, detail = "SBE19")

          lon <- mean(Select$lon)
          lat <- mean(Select$lat)

          SBE19 <- L1a$SBE19() %>% filter(date_time %within% timeInt)

          if (nrow(SBE19) == 0) {
            warning(
              paste0("SBE19 data not found at time interval: ", timeInt)
            )
          } else if (is.null(cal_data$CalSBE19()) | is.null(cal_data$CalSBE18()) | is.null(cal_data$CalSBE43())) {
            warning(
              "SBE19 | SBE18 | SBE43 calibration data not loaded"
            )
          } else {
            SBE19 <- cal_sbe19(SBE19, lon, lat) %>%
              mutate(
                oxygen_concentration = cal_sbe43( # oxygen_concentration in ml/l multiply by 1.42903 to get mg/l
                  Volt = volt0,
                  Tcelsius = temperature,
                  pressure = pressure,
                  oxygen_solubility = oxygen_solubility,
                  cal_data = cal_data$CalSBE43()
                )
              ) %>%
              mutate(
                ph = cal_sbe18(
                  Volt = Volt2,
                  Tcelsius = temperature,
                  cal_data = cal_data$CalSBE18()
                )
              ) %>%
              select(
                date_time,
                temperature,
                conductivity,
                pressure,
                salinity_practical,
                salinity_absolute,
                conservative_temperature,
                oxygen_solubility,
                oxygen_solubility,
                oxygen_concentration,
                ph
              ) %>%
              mutate(
                id = seq_along(rownames(SBE19)),
                qc = "1"
              )

            Obs$SBE19$L1b <- SBE19 %>%
              select(!any_of(c("conductivity", "conservative_temperature", "oxygen_solubility"))) %>%
              pivot_longer(
                cols = any_of(c("temperature", "pressure", "salinity_practical", "salinity_absolute", "oxygen_solubility", "oxygen_concentration", "ph")),
                names_to = "parameter",
                values_to = "value"
              ) %>%
              group_by(parameter) %>%
              nest(Data = !matches("parameter"))
          }
        }


        # HOCR L1b ----------------------------------------------------------------

        if (any(str_detect(L1a$instrumentList(), "HOCR"))) {
          # Create a callback function to update progress.
          # Each time this is called:
          # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
          #   distance. If non-NULL, it will set the progress to that value.
          # - It also accepts optional detail text.
          update_progress <- function(value = NULL, message = NULL, detail = NULL) {
            if (is.null(value)) {
              value <- progress$getValue()
              value <- value + (progress$getMax() - value) / 5
            }
            progress$set(value = value, message = message, detail = detail)
          }

          progress$set(value = 0.1, detail = "HOCR")

          Filthocr_raw <- filter_hocr(L1a$HOCR(), L1a$HOCRtimeIndex(), timeInt)

          if (length(Filthocr_raw) == 0) {
            warning(
              paste0("HOCR data not found at time interval: ", timeInt)
            )
          } else if (is.null(cal_data$hocr_cal())) {
            warning(
              "HOCR calibration data not loaded"
            )
          } else {
            # Select nearest dark data
            Obstime <- int_end(timeInt / 2)

            hocr_dark <- L1a$hocr_dark() %>%
              mutate(dark_cal_data = purrr::map(cal_data, ~ .x[which.min(abs(ymd_hms(.x$date_time) - Obstime)), ])) %>%
              ungroup() %>%
              select(sn, dark_cal_data)

            wave_seq <- seq(
              Settings$HOCR$WaveMin(),
              Settings$HOCR$WaveMax(),
              Settings$HOCR$WaveStep()
            )

            Obs$HOCR$L1b <- spsComps::shinyCatch(
              hocr_l1b(
                hocr_raw = Filthocr_raw,
                hocr_cal = cal_data$hocr_cal(),
                hocr_dark = hocr_dark,
                metadata_l2 = Obs$metadata_l2,
                update_progress,
                wave_seq = wave_seq
              ),
              shiny = T,
              trace_back = TRUE
            )
          }
        }

        # SeaOWL L1b --------------------------------------------------------------

        if (any(str_detect(L1a$instrumentList(), "SeaOWL"))) {
          progress$set(value = 0.4, detail = "SeaOWL")

          SeaOWL <- L1a$SeaOWL() %>% filter(date_time %within% timeInt)

          if (nrow(SeaOWL) == 0) {
            warning(
              paste0("SeaOWL data not found at time interval: ", timeInt)
            )
          } else if (is.null(cal_data$CalSeaOWL())) {
            warning(
              "SeaOWL calibration data not loaded"
            )
          } else {
            seaowl_l1b <- cal_seaowl(SeaOWL, cal_data$CalSeaOWL()) %>%
              mutate(
                id = seq_along(rownames(SeaOWL)),
                qc = "1"
              )

            Obs$SeaOWL$L1b <- seaowl_l1b %>%
              select(!any_of(c("sn"))) %>%
              pivot_longer(
                cols = any_of(c("vsf_700", "chl", "fdom")),
                names_to = "parameter",
                values_to = "value"
              ) %>%
              group_by(parameter) %>%
              nest(Data = !matches("parameter"))
          }
        }

        # BBFL2 L1b ---------------------------------------------------------------

        if (any(str_detect(L1a$instrumentList(), "BBFL2"))) {
          progress$set(value = 0.5, detail = "BBFL2")

          BBFL2 <- L1a$BBFL2() %>% filter(date_time %within% timeInt)

          if (nrow(BBFL2) == 0) {
            warning(
              paste0("BBFL2 data not found at time interval: ", timeInt)
            )
          } else if (is.null(cal_data$CalBBFL2())) {
            warning(
              "BBFL2 calibration data not loaded"
            )
          } else {
            bbfl2_l1b <- cal_bbfl2(BBFL2, cal_data$CalBBFL2()) %>%
              mutate(
                id = seq_along(rownames(BBFL2)),
                qc = "1"
              )

            Obs$BBFL2$L1b <- bbfl2_l1b %>%
              pivot_longer(
                cols = any_of(c("ntu", "pe", "pc")),
                names_to = "parameter",
                values_to = "value"
              ) %>%
              group_by(parameter) %>%
              nest(Data = !matches("parameter"))
          }
        }

        # BioSonic L1b ---------------------------------------------------------------

        if (any(str_detect(L1a$instrumentList(), "BioSonic"))) {
          progress$set(value = 0.6, detail = "BioSonic")

          biosonic_l1b <- L1a$BioSonic() %>% filter(date_time %within% timeInt)

          if (nrow(biosonic_l1b) == 0) {
            warning(
              paste0("BioSonic data not found at time interval: ", timeInt)
            )
          } else {
            Obs$BioSonic$L1b <- biosonic_l1b %>%
              rename(lon = longitude_deg, lat = latitude_deg) %>%
              select(
                lon,
                lat,
                date_time,
                altitude_m,
                bottom_elevation_m,
                plant_height_m,
                percent_coverage
              )
            # mutate(
            #   id = seq_along(rownames(biosonic_l1b)),
            #   qc = "1"
            # )
          }
        }


        # HydroBall L1b -----------------------------------------------------------

        if (any(str_detect(L1a$instrumentList(), "HydroBall"))) {
          progress$set(value = 0.6, detail = "HydroBall")

          hydroball_l1b <- L1a$HBDevices() %>% filter(date_time %within% timeInt)

          if (nrow(hydroball_l1b) == 0) {
            warning(
              paste0("HydroBall data not found at time interval: ", timeInt)
            )
          } else {
            Obs$HydroBall$L1b <- hydroball_l1b %>%
              rename(height_watercolumn = DBT_meter) %>%
              mutate(
                height_watercolumn = if_else(height_watercolumn == 0, NA, height_watercolumn),
                height_watercolumn = -height_watercolumn
              ) %>%
              select(
                lon,
                lat,
                date_time,
                altitude,
                height_watercolumn
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
