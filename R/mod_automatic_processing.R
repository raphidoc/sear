#' automatic_processing UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_automatic_processing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("AutoProcess"))
  )
}

#' automatic_processing Server Functions
#'
#' @noRd
mod_automatic_processing_server <- function(id, L1a, L1aSelect, cal_data, Obs, Settings, MainLog, DB) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$AutoProcess <- renderUI({
      req(L1a$ParsedFiles())

      tagList(
        actionButton(ns("AutoProcess"), "Auto Process")
      )
    })

    Trigger <- reactiveVal(NULL)
    timeInt <- reactiveVal()

    observeEvent(
      input$AutoProcess,
      label = "AutoProcess",
      {
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # discretize_time(L1a$Apla()$date_time)

        Maintime <- L1aSelect$SubMainLog()$date_time

# Create ensemble of 10 seconds -------------------------------------------

        ensemble_main <- unique(make_ensemble(Maintime))

        ensemble_metadata <- make_ensemble(MainLog()$date_time)

        ensemble_sbe19 <- make_ensemble(L1a$SBE19()$date_time)

        ensemble_seaowl <- make_ensemble(L1a$SeaOWL()$date_time)

        ensemble_bbfl2 <- make_ensemble(L1a$BBFL2()$date_time)

        ensemble_biosonic <- make_ensemble(L1a$BioSonic()$date_time)

        ensemble_hbdevices <- make_ensemble(L1a$HBDevices()$date_time)

        ensemble_hocr <- make_ensemble(L1a$HOCRtimeIndex())


# Process each ensemble ---------------------------------------------------

        library(parallel)

        i = 0
        for (ensemble in ensemble_main) {
          i = i + 1
          message(paste0("Processing ensemble ", i, " / ", length(ensemble_main)))

          metadata <- MainLog()[which(ensemble_metadata %in% ensemble), ]

          # Create metadata for the selected L1a points
          Obsuuid_l2 <- uuid::UUIDgenerate(
            use.time = T,
            output = "string"
          )

          # First have to write metadata_l2 in which uuid_l2 primary key
          # is the reference for uuid_l2 foreign key in all the other tables
          # Otherwise it is a FOREIGN KEY constraint violation
          Obs$metadata_l2 <- gen_metadataL2(metadata, ensemble)
          metadata_l2 <- Obs$metadata_l2 %>%
            mutate(
              uuid_l2 = Obsuuid_l2,
              date_time_processing = as.character(as.POSIXlt(Sys.time(), tz = "utc")),
              analyst = "Raphael Mabit",
              mail = "raphael.mabit@gmail.com"
            )
          DBI::dbWriteTable(DB$Con(), "metadata_l2", metadata_l2, append = TRUE)

          Obs$metadata_l1b <- gen_metadataL1b(metadata, ensemble)
          metadata_l1b <- Obs$metadata_l1b %>%
            mutate(
              uuid_l2 = Obsuuid_l2
            )
          DBI::dbWriteTable(DB$Con(), "metadata_l1b", metadata_l1b, append = TRUE)

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

          # CTD processing ----------------------------------------------------------

          if (any(str_detect(L1a$instrumentList(), "SBE19"))) {
            progress$set(message = "Processing L1b: ", value = progress$getValue())

            progress$set(value = 0.3, detail = "SBE19")

            lon <- mean(metadata$lon)
            lat <- mean(metadata$lat)

            sbe19 <- L1a$SBE19()[which(ensemble_sbe19 %in% ensemble), ]

            if (nrow(sbe19) == 0) {
              warning(
                paste0("SBE19 data not found at time interval: ", ensemble)
              )
            } else if (is.null(cal_data$CalSBE19()) | is.null(cal_data$CalSBE18()) | is.null(cal_data$CalSBE43())) {
              warning(
                "SBE19 | SBE18 | SBE43 calibration data not loaded"
              )
            } else {
              sbe19 <- cal_sbe19(sbe19, lon, lat) %>%
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
                  id = seq_along(rownames(sbe19)),
                  qc = "1"
                )

              Obs$SBE19$L1b <- sbe19 %>%
                select(!any_of(c("conductivity", "conservative_temperature", "oxygen_solubility"))) %>%
                pivot_longer(
                  cols = any_of(c("temperature", "pressure", "salinity_practical", "salinity_absolute", "oxygen_solubility", "oxygen_concentration", "ph")),
                  names_to = "parameter",
                  values_to = "value"
                ) %>%
                group_by(parameter) %>%
                nest(Data = !matches("parameter"))

              Obs$SBE19$L2 <- L2_param_val(Obs$SBE19$L1b)

              # Save to DB

              sbe19_l1b <- Obs$SBE19$L1b %>%
                unnest(c(Data)) %>%
                mutate(uuid_l2 = Obsuuid_l2)

              sbe19_l2 <- Obs$SBE19$L2 %>%
                mutate(uuid_l2 = Obsuuid_l2)

              DBI::dbWriteTable(DB$Con(), "sbe19_l1b", sbe19_l1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "sbe19_l2", sbe19_l2, append = TRUE)
            }
          }

          # HOCR L1 processing ------------------------------------------------------

          hocr <- L1a$HOCR()[which(ensemble_hocr %in% ensemble)]

          hocr_dark <- L1a$hocr_dark() %>%
            mutate(
              dark_cal_data = purrr::map(
                cal_data,
                ~ .x[which.min(abs(as.numeric(ymd_hms(.x$date_time)) - ensemble)), ]
              )
            ) %>%
            ungroup() %>%
            select(sn, dark_cal_data)

          wave_seq <- seq(
            Settings$HOCR$WaveMin(),
            Settings$HOCR$WaveMax(),
            Settings$HOCR$WaveStep()
          )

          # update_progress <- function(value = NULL, message = NULL, detail = NULL) {
          #   if (is.null(value)) {
          #     value <- progress$getValue()
          #     value <- value + (progress$getMax() - value) / 5
          #   }
          #   progress$set(value = value, message = message, detail = detail)
          # }

          Obs$HOCR$L1a <- spsComps::shinyCatch(
            hocr_l1a(
              hocr_raw = hocr,
              hocr_cal = cal_data$hocr_cal(),
              metadata_l2 = Obs$metadata_l2,
              update_progress = NULL
            ),
            shiny = T,
            trace_back = TRUE
          )

          # Save L1a data to table
          Obs$HOCR$L1b <- spsComps::shinyCatch(
            hocr_l1b(
              hocr_l1a = Obs$HOCR$L1a,
              hocr_dark = hocr_dark,
              wave_seq,
              update_progress = NULL
            ),
            shiny = T,
            trace_back = TRUE
          )

          if (is.null(Obs$HOCR$L1b)) {
            # Delete the outdated metadata initially created
            #  Necessarry to respect uuid_l2 foreign key rule (present in main table)
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l1b WHERE uuid_l2 = "', Obsuuid_l2, '";'))
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l2 WHERE uuid_l2 = "', Obsuuid_l2,'";'))

            next
          }

          # Filter out of water ensemble --------------------------------------------
          # Ensemble where the closest radiance value to 800 nm exceeds 0.01 sd

          df <- Obs$HOCR$L1b %>%
            filter(as.character(sn) %in% c("1413", "1415", "0237")) %>%
            unnest(cols = c(cal_data)) %>%
            filter(
              abs(wavelength - 800) == min(abs(wavelength - 800))
            ) %>%
            summarize(
              sd_800 = sd(channel, na.rm = T)
            )

          if (df$sd_800 > 0.01) {
            message("sd Lu(z1, 800) > 0.01, skipping out of water ensemble")

            # Delete the outdated metadata initially created
            #  Necessarry to respect uuid_l2 foreign key rule (present in main table)
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l1b WHERE uuid_l2 = "', Obsuuid_l2, '";'))
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l2 WHERE uuid_l2 = "', Obsuuid_l2,'";'))

            next
          }

          # HOCR L2 processing ------------------------------------------------------

          if (
            any(str_detect(L1a$instrumentList(), "SBE19")) &&
            any(Obs$SBE19$L1b$parameter == "pressure")
            ) {
            message("Taking z1 from CTD")

            pressure <- Obs$SBE19$L1b$Data[Obs$SBE19$L1b$parameter == "pressure"][[1]]$value

            ctd <- unnest(Obs$SBE19$L1b, cols = Data) %>%
              pivot_wider(
                names_from = parameter,
                values_from = value
              )

            if (any(pressure < 0)) {
              message("CTD in air, taking single z1 from setting")
              ctd <- tibble(
                z1 = Settings$HOCR$Z1Depth()
              )
            } else {
              ctd <- ctd %>%
                mutate(
                  z = gsw::gsw_z_from_p(p = pressure, latitude = metadata_l2$lat),
                  z1 = z
                )
            }

          } else {
            message("Taking single z1 from setting")
            ctd <- tibble(
              z1 = Settings$HOCR$Z1Depth()
            )
          }

          z2z1 <- Settings$HOCR$Z1Z2Depth()

          Obs$HOCR$L2 <- tryCatch(
            {
              hocr_l2(
                Obs$HOCR$L1b, wave_seq, ctd, z2z1,
                F, 0.1, Obs
              )
            },
            error = function(e) e
          )

          if (inherits(Obs$HOCR$L2, "error")) {
            message("Failled L2 processing:")
            # Passing the error actually triggers it ! who would have guess !?
            #message(Obs$HOCR$L2)

            # Delete the outdated metadata initially created
            #  Necessarry to respect uuid_l2 foreign key rule (present in main table)
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l1b WHERE uuid_l2 = "', Obsuuid_l2, '";'))
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l2 WHERE uuid_l2 = "', Obsuuid_l2,'";'))

            next
          }

          hocr_l1a <- Obs$HOCR$L1a %>%
            select(instrument, sn, raw_data) %>%
            unnest(cols = c(raw_data)) %>%
            mutate(uuid_l2 = Obsuuid_l2)

          hocr_l1b <- Obs$HOCR$L1b %>%
            unnest(cols = c(cal_data)) %>%
            mutate(uuid_l2 = Obsuuid_l2)

          hocr_l2 <- Obs$HOCR$L2 %>%
            mutate(uuid_l2 = Obsuuid_l2)

          DBI::dbWriteTable(DB$Con(), "hocr_l1a", hocr_l1a, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "hocr_l1b", hocr_l1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "hocr_l2", hocr_l2, append = TRUE)


          # SeaOWL L1b --------------------------------------------------------------

          if (any(str_detect(L1a$instrumentList(), "SeaOWL"))) {
            progress$set(value = 0.4, detail = "SeaOWL")

            SeaOWL <- L1a$SeaOWL()[which(ensemble_seaowl %in% ensemble), ]

            if (nrow(SeaOWL) == 0) {
              warning(
                paste0("SeaOWL data not found at time interval: ", ensemble)
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

              Obs$SeaOWL$L2 <- L2_param_val(Obs$SeaOWL$L1b)

              seaowl_l1b <- Obs$SeaOWL$L1b %>%
                unnest(c(Data)) %>%
                mutate(uuid_l2 = Obsuuid_l2)

              seaowl_l2 <- Obs$SeaOWL$L2 %>%
                mutate(uuid_l2 = Obsuuid_l2)

              DBI::dbWriteTable(DB$Con(), "seaowl_l1b", seaowl_l1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "seaowl_l2", seaowl_l2, append = TRUE)
            }
          }

          # BBFL2 L1b ---------------------------------------------------------------

          if (any(str_detect(L1a$instrumentList(), "BBFL2"))) {
            progress$set(value = 0.5, detail = "BBFL2")

            BBFL2 <- L1a$BBFL2()[which(ensemble_bbfl2 %in% ensemble), ]

            if (nrow(BBFL2) == 0) {
              warning(
                paste0("BBFL2 data not found at time interval: ", ensemble)
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

              Obs$BBFL2$L2 <- L2_param_val(Obs$BBFL2$L1b)

              bbfl2_l1b <- Obs$BBFL2$L1b %>%
                unnest(c(Data)) %>%
                mutate(uuid_l2 = Obsuuid_l2)

              bbfl2_l2 <- Obs$BBFL2$L2 %>%
                mutate(uuid_l2 = Obsuuid_l2)

              DBI::dbWriteTable(DB$Con(), "bbfl2_l1b", bbfl2_l1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "bbfl2_l2", bbfl2_l2, append = TRUE)
            }
          }

          # BioSonic L1b ---------------------------------------------------------------

          if (any(str_detect(L1a$instrumentList(), "BioSonic"))) {
            progress$set(value = 0.6, detail = "BioSonic")

            biosonic_l1b <- L1a$BioSonic()[which(ensemble_biosonic %in% ensemble), ]

            if (nrow(biosonic_l1b) == 0) {
              warning(
                paste0("BioSonic data not found at ensemble: ", ensemble)
              )
            } else {
              Obs$BioSonic$L1b <- biosonic_l1b %>%
                rename(lon = longitude_deg, lat = latitude_deg) %>%
                select(
                  lon,
                  lat,
                  date_time,
                  altitude_mReMsl,
                  bottom_elevation_m,
                  plant_height_m,
                  percent_coverage
                )
              # mutate(
              #   id = seq_along(rownames(biosonic_l1b)),
              #   qc = "1"
              # )

              test <- Obs$BioSonic$L1b %>% summarise(
                lon = mean(lon),
                lat = mean(lat),
                date_time = mean(date_time),
                altitude_mReMsl = mean(altitude_mReMsl),
                bottom_elevation_m = mean(bottom_elevation_m),
                plant_height_m = mean(plant_height_m),
                percent_coverage = mean(percent_coverage)
              )

              Obs$BioSonic$L2 <- test

              biosonic_l1b <- Obs$BioSonic$L1b %>%
                mutate(uuid_l2 = Obsuuid_l2)

              biosonic_l2 <- Obs$BioSonic$L2 %>%
                mutate(uuid_l2 = Obsuuid_l2)

              DBI::dbWriteTable(DB$Con(), "biosonic_l1b", biosonic_l1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "biosonic_l2", biosonic_l2, append = TRUE)
            }
          }

          # HydroBall L1b -----------------------------------------------------------

          if (any(str_detect(L1a$instrumentList(), "HydroBall"))) {
            progress$set(value = 0.6, detail = "HydroBall")

            hydroball_l1b <- L1a$HBDevices()[which(ensemble_hbdevices %in% ensemble), ]

            if (nrow(hydroball_l1b) == 0) {
              warning(
                paste0("HydroBall data not found at time interval: ", ensemble)
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

              test <- Obs$HydroBall$L1b %>% summarise(
                lon = mean(lon, na.rm = T),
                lat = mean(lat, na.rm = T),
                date_time = mean(date_time, na.rm = T),
                altitude = mean(altitude, na.rm = T),
                height_watercolumn = mean(height_watercolumn, na.rm = T)
              )

              Obs$HydroBall$L2 <- test

              hydroball_l1b <- Obs$HydroBall$L1b %>%
                mutate(
                  uuid_l2 = Obsuuid_l2
                )

              hydroball_l2 <- Obs$HydroBall$L2 %>%
                mutate(uuid_l2 = Obsuuid_l2)

              DBI::dbWriteTable(DB$Con(), "hydroball_l1b", hydroball_l1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "hydroball_l2", hydroball_l2, append = TRUE)
            }
          }
        }

# Old ---------------------------------------------------------------------

    #     UpLimit <- 10 # Seconds
    #     LowLimit <- 4
    #
    #     i <- 1
    #     j <- 1
    #     n <- 1
    #     while (T) {
    #       if (length(Maintime) < i + 1) {
    #         message("This is the end")
    #         break
    #       }
    #
    #       x <- Maintime[i]
    #       y <- Maintime[j + 1]
    #
    #       # Set upper limit for time difference
    #       if (interval(x, y) > seconds(UpLimit)) {
    #         i <- j + 1
    #         message(paste0("Reached time discretization upper limit: ", UpLimit, "S"))
    #         next
    #       }
    #
    #       # Step 1, iter until time diff reach lower boundary
    #       # If already the case no need to iter
    #
    #       if (interval(x, y) < seconds(LowLimit)) {
    #         while (interval(x, y) < seconds(LowLimit)) {
    #           if (length(Maintime) < j + 1) {
    #             message("This is the end")
    #             break
    #           }
    #
    #           if (interval(x, y) > seconds(UpLimit)) {
    #             i <- j + 1
    #             message("next next")
    #             next
    #           }
    #
    #           j <- j + 1
    #           y <- Maintime[j]
    #         }
    #       } else {
    #         j <- j + 1
    #       }
    #
    #       # At this point. this should always be true
    #       # Step 3 and 4, processing should happen at this stage
    #       if (interval(x, y) < seconds(UpLimit) & interval(x, y) >= seconds(LowLimit)) {
    #
    #         timeInt <- interval(x, y)
    #
    #         progress$set(message = paste("Processing obs :", n, timeInt), value = n / (length(Maintime) / 4))
    #
    #         message(paste("Processing obs:", n, timeInt))
    #
    #         Select <- MainLog()[(MainLog()$date_time %within% timeInt), ]
    #
    #         # Create metadata for the selected L1a points
    #         Obsuuid_l2 <- uuid::UUIDgenerate(
    #           use.time = T,
    #           output = "string"
    #         )
    #
    #         # First have to write metadata_l2 in which uuid_l2 primary key
    #         # is the reference for uuid_l2 foreign key in all the other tables
    #         # Otherwise it is a FOREIGN KEY constraint violation
    #         Obs$metadata_l2 <- gen_metadataL2(Select = Select)
    #         metadata_l2 <- Obs$metadata_l2 %>%
    #           mutate(
    #             uuid_l2 = Obsuuid_l2,
    #             date_time_processing = as.character(as.POSIXlt(Sys.time(), tz = "utc")),
    #             analyst = "Raphael Mabit",
    #             mail = "raphael.mabit@gmail.com"
    #           )
    #         DBI::dbWriteTable(DB$Con(), "metadata_l2", metadata_l2, append = TRUE)
    #
    #         Obs$metadata_l1b <- gen_metadataL1b(Select = Select)
    #         metadata_l1b <- Obs$metadata_l1b %>%
    #           mutate(
    #             uuid_l2 = Obsuuid_l2
    #           )
    #         DBI::dbWriteTable(DB$Con(), "metadata_l1b", metadata_l1b, append = TRUE)
    #
    #         # Empty L1b and L2 on new processing to avoid confusion
    #         Obs$HOCR$L1b <- tibble()
    #         Obs$SBE19$L1b <- tibble()
    #         Obs$SeaOWL$L1b <- tibble()
    #         Obs$BBFL2$L1b <- tibble()
    #         Obs$BioSonic$L1b <- tibble()
    #
    #         Obs$HOCR$L2 <- tibble()
    #         Obs$SBE19$L2 <- tibble()
    #         Obs$SeaOWL$L2 <- tibble()
    #         Obs$BBFL2$L2 <- tibble()
    #         Obs$BioSonic$L2 <- tibble()
    #
    #
    #         # HOCR --------------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$instrumentList(), "HOCR"))) {
    #           update_progress <- function(value = NULL, message = NULL, detail = NULL) {
    #             if (is.null(value)) {
    #               value <- progress$getValue()
    #               value <- value + (progress$getMax() - value) / 5
    #             }
    #             progress$set(value = value, message = message, detail = detail)
    #           }
    #
    #           Filthocr_raw <- filter_hocr(L1a$HOCR(), L1a$HOCRtimeIndex(), timeInt)
    #
    #           if (length(Filthocr_raw) == 0) {
    #             warning(
    #               paste0("HOCR data not found at time interval: ", timeInt)
    #             )
    #           } else if (is.null(cal_data$hocr_cal())) {
    #             warning(
    #               "HOCR calibration data not loaded"
    #             )
    #           } else {
    #             # Select nearest dark data
    #             Obstime <- int_end(timeInt / 2)
    #
    #             hocr_dark <- L1a$hocr_dark() %>%
    #               mutate(DarkAproxData = purrr::map(AproxData, ~ .x[which.min(abs(ymd_hms(.x$date_time) - Obstime)), ])) %>%
    #               ungroup() %>%
    #               select(sn, DarkAproxData)
    #
    #             wave_seq <- seq(
    #               Settings$HOCR$WaveMin(),
    #               Settings$HOCR$WaveMax(),
    #               Settings$HOCR$WaveStep()
    #             )
    #
    #             Obs$HOCR$L1b <- spsComps::shinyCatch(
    #               hocr_l1b(
    #                 hocr_raw = Filthocr_raw,
    #                 hocr_cal = cal_data$hocr_cal(),
    #                 hocr_dark = hocr_dark,
    #                 metadata_l1b = Obs$metadata_l1b,
    #                 update_progress,
    #                 wave_seq
    #               ),
    #               shiny = T,
    #               trace_back = TRUE
    #             )
    #
    #             # L2
    #
    #             Z1Depth <- Settings$HOCR$Z1Depth()
    #             Z1Z2Depth <- Settings$HOCR$Z1Z2Depth()
    #
    #             Obs$HOCR$L2 <- tryCatch(
    #               {
    #                 hocr_l2(
    #                   Obs$HOCR$L1b, wave_seq, Z1Depth, Z1Z2Depth,
    #                   T, 0.1, Obs
    #                 )
    #               },
    #               error = function(e) e
    #             )
    #
    #             if (inherits(Obs$HOCR$L2, "error")) {
    #               # message(Obs$HOCR$L2)
    #               message("Sear is trying to take the next j point")
    #
    #               # Delete the outdated metadata initially created
    #               #  Necessarry to respect uuid_l2 foreign key rule (present in main table)
    #               DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l1b WHERE uuid_l2 = "', Obsuuid_l2, '";'))
    #               DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM metadata_l2 WHERE uuid_l2 = "', Obsuuid_l2,'";'))
    #
    #               i <- i
    #               j <- j + 1
    #
    #               next
    #             }
    #
    #             hocr_l1b <- Obs$HOCR$L1b %>%
    #               unnest(cols = c(AproxData)) %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             hocr_l2 <- Obs$HOCR$L2 %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             DBI::dbWriteTable(DB$Con(), "hocr_l1b", hocr_l1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "hocr_l2", hocr_l2, append = TRUE)
    #           }
    #         }
    #
    #         # SBE19 L1b ---------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$instrumentList(), "SBE19"))) {
    #           progress$set(message = "Processing L1b: ", value = progress$getValue())
    #
    #           progress$set(value = 0.3, detail = "SBE19")
    #
    #           lon <- mean(Select$lon)
    #           lat <- mean(Select$lat)
    #
    #           SBE19 <- L1a$SBE19() %>% filter(date_time %within% timeInt)
    #
    #           if (nrow(SBE19) == 0) {
    #             warning(
    #               paste0("SBE19 data not found at time interval: ", timeInt)
    #             )
    #           } else if (is.null(cal_data$CalSBE19()) | is.null(cal_data$CalSBE18()) | is.null(cal_data$CalSBE43())) {
    #             warning(
    #               "SBE19 | SBE18 | SBE43 calibration data not loaded"
    #             )
    #           } else {
    #             SBE19 <- cal_sbe19(SBE19, lon, lat) %>%
    #               mutate(
    #                 oxygen_concentration = cal_sbe43( # oxygen_concentration in ml/l multiply by 1.42903 to get mg/l
    #                   Volt = volt0,
    #                   Tcelsius = temperature,
    #                   pressure = pressure,
    #                   oxygen_solubility = oxygen_solubility,
    #                   cal_data = cal_data$CalSBE43()
    #                 )
    #               ) %>%
    #               mutate(
    #                 ph = cal_sbe18(
    #                   Volt = Volt2,
    #                   Tcelsius = temperature,
    #                   cal_data = cal_data$CalSBE18()
    #                 )
    #               ) %>%
    #               select(
    #                 date_time,
    #                 temperature,
    #                 conductivity,
    #                 pressure,
    #                 salinity_practical,
    #                 salinity_absolute,
    #                 conservative_temperature,
    #                 oxygen_solubility,
    #                 oxygen_solubility,
    #                 oxygen_concentration,
    #                 ph
    #               ) %>%
    #               mutate(
    #                 id = seq_along(rownames(SBE19)),
    #                 qc = "1"
    #               )
    #
    #             Obs$SBE19$L1b <- SBE19 %>%
    #               select(!any_of(c("conductivity", "conservative_temperature", "oxygen_solubility"))) %>%
    #               pivot_longer(
    #                 cols = any_of(c("temperature", "pressure", "salinity_practical", "salinity_absolute", "oxygen_solubility", "oxygen_concentration", "ph")),
    #                 names_to = "parameter",
    #                 values_to = "value"
    #               ) %>%
    #               group_by(parameter) %>%
    #               nest(Data = !matches("parameter"))
    #
    #             Obs$SBE19$L2 <- L2_param_val(Obs$SBE19$L1b)
    #
    #             # Save to DB
    #
    #             sbe19_l1b <- Obs$SBE19$L1b %>%
    #               unnest(c(Data)) %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             sbe19_l2 <- Obs$SBE19$L2 %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             DBI::dbWriteTable(DB$Con(), "sbe19_l1b", sbe19_l1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "sbe19_l2", sbe19_l2, append = TRUE)
    #           }
    #         }
    #
    #         # SeaOWL L1b --------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$instrumentList(), "SeaOWL"))) {
    #           progress$set(value = 0.4, detail = "SeaOWL")
    #
    #           SeaOWL <- L1a$SeaOWL() %>% filter(date_time %within% timeInt)
    #
    #           if (nrow(SeaOWL) == 0) {
    #             warning(
    #               paste0("SeaOWL data not found at time interval: ", timeInt)
    #             )
    #           } else if (is.null(cal_data$CalSeaOWL())) {
    #             warning(
    #               "SeaOWL calibration data not loaded"
    #             )
    #           } else {
    #             seaowl_l1b <- cal_seaowl(SeaOWL, cal_data$CalSeaOWL()) %>%
    #               mutate(
    #                 id = seq_along(rownames(SeaOWL)),
    #                 qc = "1"
    #               )
    #
    #             Obs$SeaOWL$L1b <- seaowl_l1b %>%
    #               select(!any_of(c("sn"))) %>%
    #               pivot_longer(
    #                 cols = any_of(c("vsf_700", "chl", "fdom")),
    #                 names_to = "parameter",
    #                 values_to = "value"
    #               ) %>%
    #               group_by(parameter) %>%
    #               nest(Data = !matches("parameter"))
    #
    #             Obs$SeaOWL$L2 <- L2_param_val(Obs$SeaOWL$L1b)
    #
    #             seaowl_l1b <- Obs$SeaOWL$L1b %>%
    #               unnest(c(Data)) %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             seaowl_l2 <- Obs$SeaOWL$L2 %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             DBI::dbWriteTable(DB$Con(), "seaowl_l1b", seaowl_l1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "seaowl_l2", seaowl_l2, append = TRUE)
    #           }
    #         }
    #
    #         # BBFL2 L1b ---------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$instrumentList(), "BBFL2"))) {
    #           progress$set(value = 0.5, detail = "BBFL2")
    #
    #           BBFL2 <- L1a$BBFL2() %>% filter(date_time %within% timeInt)
    #
    #           if (nrow(BBFL2) == 0) {
    #             warning(
    #               paste0("BBFL2 data not found at time interval: ", timeInt)
    #             )
    #           } else if (is.null(cal_data$CalBBFL2())) {
    #             warning(
    #               "BBFL2 calibration data not loaded"
    #             )
    #           } else {
    #             bbfl2_l1b <- cal_bbfl2(BBFL2, cal_data$CalBBFL2()) %>%
    #               mutate(
    #                 id = seq_along(rownames(BBFL2)),
    #                 qc = "1"
    #               )
    #
    #             Obs$BBFL2$L1b <- bbfl2_l1b %>%
    #               pivot_longer(
    #                 cols = any_of(c("ntu", "pe", "pc")),
    #                 names_to = "parameter",
    #                 values_to = "value"
    #               ) %>%
    #               group_by(parameter) %>%
    #               nest(Data = !matches("parameter"))
    #
    #             Obs$BBFL2$L2 <- L2_param_val(Obs$BBFL2$L1b)
    #
    #             bbfl2_l1b <- Obs$BBFL2$L1b %>%
    #               unnest(c(Data)) %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             bbfl2_l2 <- Obs$BBFL2$L2 %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             DBI::dbWriteTable(DB$Con(), "bbfl2_l1b", bbfl2_l1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "bbfl2_l2", bbfl2_l2, append = TRUE)
    #           }
    #         }
    #
    #         # BioSonic L1b ---------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$instrumentList(), "BioSonic"))) {
    #           progress$set(value = 0.6, detail = "BioSonic")
    #
    #           biosonic_l1b <- L1a$BioSonic() %>% filter(date_time %within% timeInt)
    #
    #           if (nrow(biosonic_l1b) == 0) {
    #             warning(
    #               paste0("BioSonic data not found at time interval: ", timeInt)
    #             )
    #           } else {
    #             Obs$BioSonic$L1b <- biosonic_l1b %>%
    #               rename(lon = longitude_deg, lat = latitude_deg) %>%
    #               select(
    #                 lon,
    #                 lat,
    #                 date_time,
    #                 altitude_mReMsl,
    #                 bottom_elevation_m,
    #                 plant_height_m,
    #                 percent_coverage
    #               )
    #             # mutate(
    #             #   id = seq_along(rownames(biosonic_l1b)),
    #             #   qc = "1"
    #             # )
    #
    #             test <- Obs$BioSonic$L1b %>% summarise(
    #               lon = mean(lon),
    #               lat = mean(lat),
    #               date_time = mean(date_time),
    #               altitude_mReMsl = mean(altitude_mReMsl),
    #               bottom_elevation_m = mean(bottom_elevation_m),
    #               plant_height_m = mean(plant_height_m),
    #               percent_coverage = mean(percent_coverage)
    #             )
    #
    #             Obs$BioSonic$L2 <- test
    #
    #             biosonic_l1b <- Obs$BioSonic$L1b %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             biosonic_l2 <- Obs$BioSonic$L2 %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             DBI::dbWriteTable(DB$Con(), "biosonic_l1b", biosonic_l1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "biosonic_l2", biosonic_l2, append = TRUE)
    #           }
    #         }
    #
    #         # HydroBall L1b -----------------------------------------------------------
    #
    #         if (any(str_detect(L1a$instrumentList(), "HydroBall"))) {
    #           progress$set(value = 0.6, detail = "HydroBall")
    #
    #           hydroball_l1b <- L1a$HBDevices() %>% filter(date_time %within% timeInt)
    #
    #           if (nrow(hydroball_l1b) == 0) {
    #             warning(
    #               paste0("HydroBall data not found at time interval: ", timeInt)
    #             )
    #           } else {
    #             Obs$HydroBall$L1b <- hydroball_l1b %>%
    #               rename(height_watercolumn = DBT_meter) %>%
    #               mutate(
    #                 height_watercolumn = if_else(height_watercolumn == 0, NA, height_watercolumn),
    #                 height_watercolumn = -height_watercolumn
    #               ) %>%
    #               select(
    #                 lon,
    #                 lat,
    #                 date_time,
    #                 altitude,
    #                 height_watercolumn
    #               )
    #
    #             test <- Obs$HydroBall$L1b %>% summarise(
    #               lon = mean(lon, na.rm = T),
    #               lat = mean(lat, na.rm = T),
    #               date_time = mean(date_time, na.rm = T),
    #               altitude = mean(altitude, na.rm = T),
    #               height_watercolumn = mean(height_watercolumn, na.rm = T)
    #             )
    #
    #             Obs$HydroBall$L2 <- test
    #
    #             hydroball_l1b <- Obs$HydroBall$L1b %>%
    #               mutate(
    #                 uuid_l2 = Obsuuid_l2
    #               )
    #
    #             hydroball_l2 <- Obs$HydroBall$L2 %>%
    #               mutate(uuid_l2 = Obsuuid_l2)
    #
    #             DBI::dbWriteTable(DB$Con(), "hydroball_l1b", hydroball_l1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "hydroball_l2", hydroball_l2, append = TRUE)
    #           }
    #         }
    #
    #         # Feedback to the user
    #         # session$sendCustomMessage(
    #         #   type = "testmessage",
    #         #   message = "Saved"
    #         #   # glue::glue(
    #         #   #   "Metadata: ",MetaUp," entry updated\n",
    #         #   #   "hocr_l1b: ", sum(L1bUp)," entry updated\n",
    #         #   #   "hocr_l2: ",L2Up," entry updated\n")
    #         # )
    #
    #         # qry <- glue::glue_sql("SELECT * FROM Metadata WHERE uuid_l2 = '", Obsuuid_l2, "';")
    #         #
    #         # Obs$Metadata <- tibble(DBI::dbGetQuery(DB$Con(), qry))
    #
    #         n <- n + 1
    #       }
    #
    #       # Exclude endpoints from %within%
    #       i <- j + 1
    #     }

        # Update the list of observation
        DB$ObsMeta(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM metadata_l2")))
      }
    )


    # Module output -----------------------------------------------------------
    list()
  })
}

## To be copied in the UI
# mod_automatic_processing_ui("automatic_processing")

## To be copied in the server
# mod_automatic_processing_server("automatic_processing")
