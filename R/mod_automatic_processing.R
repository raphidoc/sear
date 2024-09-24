#' automatic_processing UI Function
#'
#' @description A shiny Module.
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
mod_automatic_processing_server <- function(id, L1a, L1aSelect, CalData, Obs, Settings, MainLog, DB) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$AutoProcess <- renderUI({
      req(L1a$ParsedFiles())

      tagList(
        actionButton(ns("AutoProcess"), "Auto Process")
      )
    })

    Trigger <- reactiveVal(NULL)
    TimeInt <- reactiveVal()

    observeEvent(
      input$AutoProcess,
      label = "AutoProcess",
      {
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        # discretize_time(L1a$Apla()$DateTime)

        MainTime <- L1aSelect$SubMainLog()$DateTime

# Create ensemble of 10 seconds -------------------------------------------

        ensemble_main <- unique(make_ensemble(MainTime))

        ensemble_metadata <- make_ensemble(MainLog()$DateTime)

        ensemble_sbe19 <- make_ensemble(L1a$SBE19()$DateTime)

        ensemble_seaowl <- make_ensemble(L1a$SeaOWL()$DateTime)

        ensemble_bbfl2 <- make_ensemble(L1a$BBFL2()$DateTime)

        ensemble_biosonic <- make_ensemble(L1a$BioSonic()$DateTime)

        ensemble_hbdevices <- make_ensemble(L1a$HBDevices()$DateTime)

        ensemble_hocr <- make_ensemble(L1a$HOCRTimeIndex())


# Process each ensemble ---------------------------------------------------

        library(parallel)

        i = 0
        for (ensemble in ensemble_main) {
          i = i + 1
          message(paste0("Processing ensemble ", i, " / ", length(ensemble_main)))

          metadata <- MainLog()[which(ensemble_metadata %in% ensemble), ]

          # Create metadata for the selected L1a points
          ObsUUID <- uuid::UUIDgenerate(
            use.time = T,
            output = "string"
          )

          # First have to write MetadataL2 in which UUID primary key
          # is the reference for UUID foreign key in all the other tables
          # Otherwise it is a FOREIGN KEY constraint violation
          Obs$MetadataL2 <- gen_metadataL2(metadata, ensemble)
          MetadataL2 <- Obs$MetadataL2 %>%
            mutate(
              UUID = ObsUUID,
              ProTime = as.character(as.POSIXlt(Sys.time(), tz = "UTC")),
              Analyst = "Raphael Mabit",
              Mail = "raphael.mabit@gmail.com"
            )
          DBI::dbWriteTable(DB$Con(), "MetadataL2", MetadataL2, append = TRUE)

          Obs$MetadataL1b <- gen_metadataL1b(metadata, ensemble)
          MetadataL1b <- Obs$MetadataL1b %>%
            mutate(
              UUID = ObsUUID
            )
          DBI::dbWriteTable(DB$Con(), "MetadataL1b", MetadataL1b, append = TRUE)

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

          if (any(str_detect(L1a$InstrumentList(), "SBE19"))) {
            progress$set(message = "Processing L1b: ", value = progress$getValue())

            progress$set(value = 0.3, detail = "SBE19")

            Lon <- mean(metadata$Lon)
            Lat <- mean(metadata$Lat)

            sbe19 <- L1a$SBE19()[which(ensemble_sbe19 %in% ensemble), ]

            if (nrow(sbe19) == 0) {
              warning(
                paste0("SBE19 data not found at time interval: ", ensemble)
              )
            } else if (is.null(CalData$CalSBE19()) | is.null(CalData$CalSBE18()) | is.null(CalData$CalSBE43())) {
              warning(
                "SBE19 | SBE18 | SBE43 calibration data not loaded"
              )
            } else {
              sbe19 <- cal_sbe19(sbe19, Lon, Lat) %>%
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
                  ID = seq_along(rownames(sbe19)),
                  QC = "1"
                )

              Obs$SBE19$L1b <- sbe19 %>%
                select(!any_of(c("Conductivity", "CT", "O2Sol"))) %>%
                pivot_longer(
                  cols = any_of(c("Temperature", "Pressure", "SP", "SA", "OxSol", "Oxygen", "pH")),
                  names_to = "Parameter",
                  values_to = "Value"
                ) %>%
                group_by(Parameter) %>%
                nest(Data = !matches("Parameter"))

              Obs$SBE19$L2 <- L2_param_val(Obs$SBE19$L1b)

              # Save to DB

              SBE19L1b <- Obs$SBE19$L1b %>%
                unnest(c(Data)) %>%
                mutate(UUID = ObsUUID)

              SBE19L2 <- Obs$SBE19$L2 %>%
                mutate(UUID = ObsUUID)

              DBI::dbWriteTable(DB$Con(), "SBE19L1b", SBE19L1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "SBE19L2", SBE19L2, append = TRUE)
            }
          }

          # HOCR L1 processing ------------------------------------------------------

          hocr <- L1a$HOCR()[which(ensemble_hocr %in% ensemble)]

          hocr_dark <- L1a$HOCRDark() %>%
            mutate(
              DarkCalData = purrr::map(
                CalData,
                ~ .x[which.min(abs(as.numeric(ymd_hms(.x$DateTime)) - ensemble)), ]
              )
            ) %>%
            ungroup() %>%
            select(SN, DarkCalData)

          wave_seq <- seq(
            Settings$HOCR$WaveMin(),
            Settings$HOCR$WaveMax(),
            Settings$HOCR$WaveStep()
          )

          # UpdateProgress <- function(value = NULL, message = NULL, detail = NULL) {
          #   if (is.null(value)) {
          #     value <- progress$getValue()
          #     value <- value + (progress$getMax() - value) / 5
          #   }
          #   progress$set(value = value, message = message, detail = detail)
          # }

          Obs$HOCR$L1b <- spsComps::shinyCatch(
            cal_hocr(
              RawHOCR = hocr,
              CalHOCR = CalData$CalHOCR(),
              HOCRDark = hocr_dark,
              MetadataL2 = Obs$MetadataL2,
              UpdateProgress = NULL,
              wave_seq
            ),
            shiny = T,
            trace_back = TRUE
          )

          if (is.null(Obs$HOCR$L1b)) {
            # Delete the outdated metadata initially created
            #  Necessarry to respect UUID foreign key rule (present in main table)
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL1b WHERE UUID = "', ObsUUID, '";'))
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL2 WHERE UUID = "', ObsUUID,'";'))

            next
          }

          # Filter out of water ensemble --------------------------------------------
          # Ensemble where the closest radiance value to 800 nm exceeds 0.01 sd

          df <- Obs$HOCR$L1b %>%
            filter(as.character(SN) %in% c("1413", "1415", "0237")) %>%
            unnest(cols = c(CalData)) %>%
            filter(
              abs(Wavelength - 800) == min(abs(Wavelength - 800))
            ) %>%
            summarize(
              sd_800 = sd(Channels, na.rm = T)
            )

          if (df$sd_800 > 0.01) {
            message("sd Lu(z1, 800) > 0.01, skipping out of water ensemble")

            # Delete the outdated metadata initially created
            #  Necessarry to respect UUID foreign key rule (present in main table)
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL1b WHERE UUID = "', ObsUUID, '";'))
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL2 WHERE UUID = "', ObsUUID,'";'))

            next
          }

          # HOCR L2 processing ------------------------------------------------------

          if (
            any(str_detect(L1a$InstrumentList(), "SBE19")) &&
            any(Obs$SBE19$L1b$Parameter == "Pressure")
            ) {
            message("Taking z1 from CTD")

            pressure <- Obs$SBE19$L1b$Data[Obs$SBE19$L1b$Parameter == "Pressure"][[1]]$Value

            if (any(pressure < 0)) {
              message("CTD in air, taking single z1 from setting")
              z1 <- tibble(
                z1_median = Settings$HOCR$Z1Depth(),
                z1_sd = 0
              )
            } else {
              z1 <- tibble(
                z = gsw::gsw_z_from_p(p = pressure, latitude = MetadataL2$Lat),
                z1 = z
              ) %>%
                summarise(
                  across(where(is.numeric), list(median = median, sd = ~ sd(.x, na.rm=T)), .names= "{.col}_{.fn}")
                )
            }

          } else {
            message("Taking single z1 from setting")
            z1 <- tibble(
              z1_median = Settings$HOCR$Z1Depth(),
              z1_sd = 0
            )
          }

          z2z1 <- Settings$HOCR$Z1Z2Depth()

          Obs$HOCR$L2 <- tryCatch(
            {
              L2_hocr(
                Obs$HOCR$L1b, wave_seq, z1, z2z1,
                T, 0.1, Obs
              )
            },
            error = function(e) e
          )

          if (inherits(Obs$HOCR$L2, "error")) {
            message("Failled L2 processing:")
            # Passing the error actually triggers it ! who would have guess !?
            #message(Obs$HOCR$L2)

            # Delete the outdated metadata initially created
            #  Necessarry to respect UUID foreign key rule (present in main table)
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL1b WHERE UUID = "', ObsUUID, '";'))
            DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL2 WHERE UUID = "', ObsUUID,'";'))

            next
          }

          HOCRL1b <- Obs$HOCR$L1b %>%
            unnest(cols = c(CalData)) %>%
            mutate(UUID = ObsUUID)

          HOCRL2 <- Obs$HOCR$L2 %>%
            mutate(UUID = ObsUUID)

          DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "HOCRL2", HOCRL2, append = TRUE)


          # SeaOWL L1b --------------------------------------------------------------

          if (any(str_detect(L1a$InstrumentList(), "SeaOWL"))) {
            progress$set(value = 0.4, detail = "SeaOWL")

            SeaOWL <- L1a$SeaOWL()[which(ensemble_seaowl %in% ensemble), ]

            if (nrow(SeaOWL) == 0) {
              warning(
                paste0("SeaOWL data not found at time interval: ", ensemble)
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

              Obs$SeaOWL$L2 <- L2_param_val(Obs$SeaOWL$L1b)

              SeaOWLL1b <- Obs$SeaOWL$L1b %>%
                unnest(c(Data)) %>%
                mutate(UUID = ObsUUID)

              SeaOWLL2 <- Obs$SeaOWL$L2 %>%
                mutate(UUID = ObsUUID)

              DBI::dbWriteTable(DB$Con(), "SeaOWLL1b", SeaOWLL1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "SeaOWLL2", SeaOWLL2, append = TRUE)
            }
          }

          # BBFL2 L1b ---------------------------------------------------------------

          if (any(str_detect(L1a$InstrumentList(), "BBFL2"))) {
            progress$set(value = 0.5, detail = "BBFL2")

            BBFL2 <- L1a$BBFL2()[which(ensemble_bbfl2 %in% ensemble), ]

            if (nrow(BBFL2) == 0) {
              warning(
                paste0("BBFL2 data not found at time interval: ", ensemble)
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

              Obs$BBFL2$L2 <- L2_param_val(Obs$BBFL2$L1b)

              BBFL2L1b <- Obs$BBFL2$L1b %>%
                unnest(c(Data)) %>%
                mutate(UUID = ObsUUID)

              BBFL2L2 <- Obs$BBFL2$L2 %>%
                mutate(UUID = ObsUUID)

              DBI::dbWriteTable(DB$Con(), "BBFL2L1b", BBFL2L1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "BBFL2L2", BBFL2L2, append = TRUE)
            }
          }

          # BioSonic L1b ---------------------------------------------------------------

          if (any(str_detect(L1a$InstrumentList(), "BioSonic"))) {
            progress$set(value = 0.6, detail = "BioSonic")

            BioSonicL1b <- L1a$BioSonic()[which(ensemble_biosonic %in% ensemble), ]

            if (nrow(BioSonicL1b) == 0) {
              warning(
                paste0("BioSonic data not found at ensemble: ", ensemble)
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

              test <- Obs$BioSonic$L1b %>% summarise(
                Lon = mean(Lon),
                Lat = mean(Lat),
                DateTime = mean(DateTime),
                Altitude_mReMsl = mean(Altitude_mReMsl),
                BottomElevation_m = mean(BottomElevation_m),
                PlantHeight_m = mean(PlantHeight_m),
                PercentCoverage = mean(PercentCoverage)
              )

              Obs$BioSonic$L2 <- test

              BioSonicL1b <- Obs$BioSonic$L1b %>%
                mutate(UUID = ObsUUID)

              BioSonicL2 <- Obs$BioSonic$L2 %>%
                mutate(UUID = ObsUUID)

              DBI::dbWriteTable(DB$Con(), "BioSonicL1b", BioSonicL1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "BioSonicL2", BioSonicL2, append = TRUE)
            }
          }

          # HydroBall L1b -----------------------------------------------------------

          if (any(str_detect(L1a$InstrumentList(), "HydroBall"))) {
            progress$set(value = 0.6, detail = "HydroBall")

            HydroBallL1b <- L1a$HBDevices()[which(ensemble_hbdevices %in% ensemble), ]

            if (nrow(HydroBallL1b) == 0) {
              warning(
                paste0("HydroBall data not found at time interval: ", ensemble)
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

              test <- Obs$HydroBall$L1b %>% summarise(
                Lon = mean(Lon, na.rm = T),
                Lat = mean(Lat, na.rm = T),
                DateTime = mean(DateTime, na.rm = T),
                Altitude = mean(Altitude, na.rm = T),
                H = mean(H, na.rm = T)
              )

              Obs$HydroBall$L2 <- test

              HydroBallL1b <- Obs$HydroBall$L1b %>%
                mutate(
                  UUID = ObsUUID
                )

              HydroBallL2 <- Obs$HydroBall$L2 %>%
                mutate(UUID = ObsUUID)

              DBI::dbWriteTable(DB$Con(), "HydroBallL1b", HydroBallL1b, append = TRUE)
              DBI::dbWriteTable(DB$Con(), "HydroBallL2", HydroBallL2, append = TRUE)
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
    #       if (length(MainTime) < i + 1) {
    #         message("This is the end")
    #         break
    #       }
    #
    #       x <- MainTime[i]
    #       y <- MainTime[j + 1]
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
    #           if (length(MainTime) < j + 1) {
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
    #           y <- MainTime[j]
    #         }
    #       } else {
    #         j <- j + 1
    #       }
    #
    #       # At this point. this should always be true
    #       # Step 3 and 4, processing should happen at this stage
    #       if (interval(x, y) < seconds(UpLimit) & interval(x, y) >= seconds(LowLimit)) {
    #
    #         TimeInt <- interval(x, y)
    #
    #         progress$set(message = paste("Processing obs :", n, TimeInt), value = n / (length(MainTime) / 4))
    #
    #         message(paste("Processing obs:", n, TimeInt))
    #
    #         Select <- MainLog()[(MainLog()$DateTime %within% TimeInt), ]
    #
    #         # Create metadata for the selected L1a points
    #         ObsUUID <- uuid::UUIDgenerate(
    #           use.time = T,
    #           output = "string"
    #         )
    #
    #         # First have to write MetadataL2 in which UUID primary key
    #         # is the reference for UUID foreign key in all the other tables
    #         # Otherwise it is a FOREIGN KEY constraint violation
    #         Obs$MetadataL2 <- gen_metadataL2(Select = Select)
    #         MetadataL2 <- Obs$MetadataL2 %>%
    #           mutate(
    #             UUID = ObsUUID,
    #             ProTime = as.character(as.POSIXlt(Sys.time(), tz = "UTC")),
    #             Analyst = "Raphael Mabit",
    #             Mail = "raphael.mabit@gmail.com"
    #           )
    #         DBI::dbWriteTable(DB$Con(), "MetadataL2", MetadataL2, append = TRUE)
    #
    #         Obs$MetadataL1b <- gen_metadataL1b(Select = Select)
    #         MetadataL1b <- Obs$MetadataL1b %>%
    #           mutate(
    #             UUID = ObsUUID
    #           )
    #         DBI::dbWriteTable(DB$Con(), "MetadataL1b", MetadataL1b, append = TRUE)
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
    #         if (any(str_detect(L1a$InstrumentList(), "HOCR"))) {
    #           UpdateProgress <- function(value = NULL, message = NULL, detail = NULL) {
    #             if (is.null(value)) {
    #               value <- progress$getValue()
    #               value <- value + (progress$getMax() - value) / 5
    #             }
    #             progress$set(value = value, message = message, detail = detail)
    #           }
    #
    #           FiltRawHOCR <- filter_hocr(L1a$HOCR(), L1a$HOCRTimeIndex(), TimeInt)
    #
    #           if (length(FiltRawHOCR) == 0) {
    #             warning(
    #               paste0("HOCR data not found at time interval: ", TimeInt)
    #             )
    #           } else if (is.null(CalData$CalHOCR())) {
    #             warning(
    #               "HOCR calibration data not loaded"
    #             )
    #           } else {
    #             # Select nearest dark data
    #             ObsTime <- int_end(TimeInt / 2)
    #
    #             HOCRDark <- L1a$HOCRDark() %>%
    #               mutate(DarkAproxData = purrr::map(AproxData, ~ .x[which.min(abs(ymd_hms(.x$DateTime) - ObsTime)), ])) %>%
    #               ungroup() %>%
    #               select(SN, DarkAproxData)
    #
    #             WaveSeq <- seq(
    #               Settings$HOCR$WaveMin(),
    #               Settings$HOCR$WaveMax(),
    #               Settings$HOCR$WaveStep()
    #             )
    #
    #             Obs$HOCR$L1b <- spsComps::shinyCatch(
    #               cal_hocr(
    #                 RawHOCR = FiltRawHOCR,
    #                 CalHOCR = CalData$CalHOCR(),
    #                 HOCRDark = HOCRDark,
    #                 MetadataL1b = Obs$MetadataL1b,
    #                 UpdateProgress,
    #                 WaveSeq
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
    #                 L2_hocr(
    #                   Obs$HOCR$L1b, WaveSeq, Z1Depth, Z1Z2Depth,
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
    #               #  Necessarry to respect UUID foreign key rule (present in main table)
    #               DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL1b WHERE UUID = "', ObsUUID, '";'))
    #               DBI::dbSendQuery(DB$Con(), paste0('DELETE FROM MetadataL2 WHERE UUID = "', ObsUUID,'";'))
    #
    #               i <- i
    #               j <- j + 1
    #
    #               next
    #             }
    #
    #             HOCRL1b <- Obs$HOCR$L1b %>%
    #               unnest(cols = c(AproxData)) %>%
    #               mutate(UUID = ObsUUID)
    #
    #             HOCRL2 <- Obs$HOCR$L2 %>%
    #               mutate(UUID = ObsUUID)
    #
    #             DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "HOCRL2", HOCRL2, append = TRUE)
    #           }
    #         }
    #
    #         # SBE19 L1b ---------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$InstrumentList(), "SBE19"))) {
    #           progress$set(message = "Processing L1b: ", value = progress$getValue())
    #
    #           progress$set(value = 0.3, detail = "SBE19")
    #
    #           Lon <- mean(Select$Lon)
    #           Lat <- mean(Select$Lat)
    #
    #           SBE19 <- L1a$SBE19() %>% filter(DateTime %within% TimeInt)
    #
    #           if (nrow(SBE19) == 0) {
    #             warning(
    #               paste0("SBE19 data not found at time interval: ", TimeInt)
    #             )
    #           } else if (is.null(CalData$CalSBE19()) | is.null(CalData$CalSBE18()) | is.null(CalData$CalSBE43())) {
    #             warning(
    #               "SBE19 | SBE18 | SBE43 calibration data not loaded"
    #             )
    #           } else {
    #             SBE19 <- cal_sbe19(SBE19, Lon, Lat) %>%
    #               mutate(
    #                 Oxygen = cal_sbe43( # Oxygen in ml/l multiply by 1.42903 to get mg/l
    #                   Volt = Volt0,
    #                   Tcelsius = Temperature,
    #                   Pressure = Pressure,
    #                   OxSol = OxSol,
    #                   CalData = CalData$CalSBE43()
    #                 )
    #               ) %>%
    #               mutate(
    #                 pH = cal_sbe18(
    #                   Volt = Volt2,
    #                   Tcelsius = Temperature,
    #                   CalData = CalData$CalSBE18()
    #                 )
    #               ) %>%
    #               select(
    #                 DateTime,
    #                 Temperature,
    #                 Conductivity,
    #                 Pressure,
    #                 SP,
    #                 SA,
    #                 CT,
    #                 O2Sol,
    #                 OxSol,
    #                 Oxygen,
    #                 pH
    #               ) %>%
    #               mutate(
    #                 ID = seq_along(rownames(SBE19)),
    #                 QC = "1"
    #               )
    #
    #             Obs$SBE19$L1b <- SBE19 %>%
    #               select(!any_of(c("Conductivity", "CT", "O2Sol"))) %>%
    #               pivot_longer(
    #                 cols = any_of(c("Temperature", "Pressure", "SP", "SA", "OxSol", "Oxygen", "pH")),
    #                 names_to = "Parameter",
    #                 values_to = "Value"
    #               ) %>%
    #               group_by(Parameter) %>%
    #               nest(Data = !matches("Parameter"))
    #
    #             Obs$SBE19$L2 <- L2_param_val(Obs$SBE19$L1b)
    #
    #             # Save to DB
    #
    #             SBE19L1b <- Obs$SBE19$L1b %>%
    #               unnest(c(Data)) %>%
    #               mutate(UUID = ObsUUID)
    #
    #             SBE19L2 <- Obs$SBE19$L2 %>%
    #               mutate(UUID = ObsUUID)
    #
    #             DBI::dbWriteTable(DB$Con(), "SBE19L1b", SBE19L1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "SBE19L2", SBE19L2, append = TRUE)
    #           }
    #         }
    #
    #         # SeaOWL L1b --------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$InstrumentList(), "SeaOWL"))) {
    #           progress$set(value = 0.4, detail = "SeaOWL")
    #
    #           SeaOWL <- L1a$SeaOWL() %>% filter(DateTime %within% TimeInt)
    #
    #           if (nrow(SeaOWL) == 0) {
    #             warning(
    #               paste0("SeaOWL data not found at time interval: ", TimeInt)
    #             )
    #           } else if (is.null(CalData$CalSeaOWL())) {
    #             warning(
    #               "SeaOWL calibration data not loaded"
    #             )
    #           } else {
    #             SeaOWLL1b <- cal_seaowl(SeaOWL, CalData$CalSeaOWL()) %>%
    #               mutate(
    #                 ID = seq_along(rownames(SeaOWL)),
    #                 QC = "1"
    #               )
    #
    #             Obs$SeaOWL$L1b <- SeaOWLL1b %>%
    #               select(!any_of(c("SN"))) %>%
    #               pivot_longer(
    #                 cols = any_of(c("VSF_700", "Chl", "FDOM")),
    #                 names_to = "Parameter",
    #                 values_to = "Value"
    #               ) %>%
    #               group_by(Parameter) %>%
    #               nest(Data = !matches("Parameter"))
    #
    #             Obs$SeaOWL$L2 <- L2_param_val(Obs$SeaOWL$L1b)
    #
    #             SeaOWLL1b <- Obs$SeaOWL$L1b %>%
    #               unnest(c(Data)) %>%
    #               mutate(UUID = ObsUUID)
    #
    #             SeaOWLL2 <- Obs$SeaOWL$L2 %>%
    #               mutate(UUID = ObsUUID)
    #
    #             DBI::dbWriteTable(DB$Con(), "SeaOWLL1b", SeaOWLL1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "SeaOWLL2", SeaOWLL2, append = TRUE)
    #           }
    #         }
    #
    #         # BBFL2 L1b ---------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$InstrumentList(), "BBFL2"))) {
    #           progress$set(value = 0.5, detail = "BBFL2")
    #
    #           BBFL2 <- L1a$BBFL2() %>% filter(DateTime %within% TimeInt)
    #
    #           if (nrow(BBFL2) == 0) {
    #             warning(
    #               paste0("BBFL2 data not found at time interval: ", TimeInt)
    #             )
    #           } else if (is.null(CalData$CalBBFL2())) {
    #             warning(
    #               "BBFL2 calibration data not loaded"
    #             )
    #           } else {
    #             BBFL2L1b <- cal_bbfl2(BBFL2, CalData$CalBBFL2()) %>%
    #               mutate(
    #                 ID = seq_along(rownames(BBFL2)),
    #                 QC = "1"
    #               )
    #
    #             Obs$BBFL2$L1b <- BBFL2L1b %>%
    #               pivot_longer(
    #                 cols = any_of(c("NTU", "PE", "PC")),
    #                 names_to = "Parameter",
    #                 values_to = "Value"
    #               ) %>%
    #               group_by(Parameter) %>%
    #               nest(Data = !matches("Parameter"))
    #
    #             Obs$BBFL2$L2 <- L2_param_val(Obs$BBFL2$L1b)
    #
    #             BBFL2L1b <- Obs$BBFL2$L1b %>%
    #               unnest(c(Data)) %>%
    #               mutate(UUID = ObsUUID)
    #
    #             BBFL2L2 <- Obs$BBFL2$L2 %>%
    #               mutate(UUID = ObsUUID)
    #
    #             DBI::dbWriteTable(DB$Con(), "BBFL2L1b", BBFL2L1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "BBFL2L2", BBFL2L2, append = TRUE)
    #           }
    #         }
    #
    #         # BioSonic L1b ---------------------------------------------------------------
    #
    #         if (any(str_detect(L1a$InstrumentList(), "BioSonic"))) {
    #           progress$set(value = 0.6, detail = "BioSonic")
    #
    #           BioSonicL1b <- L1a$BioSonic() %>% filter(DateTime %within% TimeInt)
    #
    #           if (nrow(BioSonicL1b) == 0) {
    #             warning(
    #               paste0("BioSonic data not found at time interval: ", TimeInt)
    #             )
    #           } else {
    #             Obs$BioSonic$L1b <- BioSonicL1b %>%
    #               rename(Lon = Longitude_deg, Lat = Latitude_deg) %>%
    #               select(
    #                 Lon,
    #                 Lat,
    #                 DateTime,
    #                 Altitude_mReMsl,
    #                 BottomElevation_m,
    #                 PlantHeight_m,
    #                 PercentCoverage
    #               )
    #             # mutate(
    #             #   ID = seq_along(rownames(BioSonicL1b)),
    #             #   QC = "1"
    #             # )
    #
    #             test <- Obs$BioSonic$L1b %>% summarise(
    #               Lon = mean(Lon),
    #               Lat = mean(Lat),
    #               DateTime = mean(DateTime),
    #               Altitude_mReMsl = mean(Altitude_mReMsl),
    #               BottomElevation_m = mean(BottomElevation_m),
    #               PlantHeight_m = mean(PlantHeight_m),
    #               PercentCoverage = mean(PercentCoverage)
    #             )
    #
    #             Obs$BioSonic$L2 <- test
    #
    #             BioSonicL1b <- Obs$BioSonic$L1b %>%
    #               mutate(UUID = ObsUUID)
    #
    #             BioSonicL2 <- Obs$BioSonic$L2 %>%
    #               mutate(UUID = ObsUUID)
    #
    #             DBI::dbWriteTable(DB$Con(), "BioSonicL1b", BioSonicL1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "BioSonicL2", BioSonicL2, append = TRUE)
    #           }
    #         }
    #
    #         # HydroBall L1b -----------------------------------------------------------
    #
    #         if (any(str_detect(L1a$InstrumentList(), "HydroBall"))) {
    #           progress$set(value = 0.6, detail = "HydroBall")
    #
    #           HydroBallL1b <- L1a$HBDevices() %>% filter(DateTime %within% TimeInt)
    #
    #           if (nrow(HydroBallL1b) == 0) {
    #             warning(
    #               paste0("HydroBall data not found at time interval: ", TimeInt)
    #             )
    #           } else {
    #             Obs$HydroBall$L1b <- HydroBallL1b %>%
    #               rename(H = DBT_meter) %>%
    #               mutate(
    #                 H = if_else(H == 0, NA, H),
    #                 H = -H
    #               ) %>%
    #               select(
    #                 Lon,
    #                 Lat,
    #                 DateTime,
    #                 Altitude,
    #                 H
    #               )
    #
    #             test <- Obs$HydroBall$L1b %>% summarise(
    #               Lon = mean(Lon, na.rm = T),
    #               Lat = mean(Lat, na.rm = T),
    #               DateTime = mean(DateTime, na.rm = T),
    #               Altitude = mean(Altitude, na.rm = T),
    #               H = mean(H, na.rm = T)
    #             )
    #
    #             Obs$HydroBall$L2 <- test
    #
    #             HydroBallL1b <- Obs$HydroBall$L1b %>%
    #               mutate(
    #                 UUID = ObsUUID
    #               )
    #
    #             HydroBallL2 <- Obs$HydroBall$L2 %>%
    #               mutate(UUID = ObsUUID)
    #
    #             DBI::dbWriteTable(DB$Con(), "HydroBallL1b", HydroBallL1b, append = TRUE)
    #             DBI::dbWriteTable(DB$Con(), "HydroBallL2", HydroBallL2, append = TRUE)
    #           }
    #         }
    #
    #         # Feedback to the user
    #         # session$sendCustomMessage(
    #         #   type = "testmessage",
    #         #   message = "Saved"
    #         #   # glue::glue(
    #         #   #   "Metadata: ",MetaUp," entry updated\n",
    #         #   #   "HOCRL1b: ", sum(L1bUp)," entry updated\n",
    #         #   #   "HOCRL2: ",L2Up," entry updated\n")
    #         # )
    #
    #         # qry <- glue::glue_sql("SELECT * FROM Metadata WHERE UUID = '", ObsUUID, "';")
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
        DB$ObsMeta(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM MetadataL2")))
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
