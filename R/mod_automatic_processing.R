#' automatic_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_automatic_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("AutoProcess"))
  )
}

#' automatic_processing Server Functions
#'
#' @noRd
mod_automatic_processing_server <- function(id, L1a, L1aSelect, CalData, Obs, Settings, MainLog, DB){
  moduleServer( id, function(input, output, session){
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

        #discretize_time(L1a$Apla()$DateTime)

        MainTime <- L1aSelect$SubMainLog()$DateTime

        UpLimit <- 10
        LowLimit <- 4

        i = 1
        j = 1
        n = 1
        while (T) {
          if (length(MainTime) < i + 1) {
            message("This is the end")
            break
          }

          x = MainTime[i]
          y = MainTime[j + 1]

          # Set upper limit for time difference
          if (interval(x, y) > seconds(UpLimit)) {
            i = j + 1
            message("next next")
            next
          }

          # Step 1, iter until time diff reach lower boundary
          # If already the case no need to iter

          if (interval(x, y) <= seconds(LowLimit)) {
            while (interval(x, y) <= seconds(LowLimit)) {
              if (length(MainTime) < j + 1) {
                message("This is the end")
                break
              }

              if (interval(x, y) > seconds(UpLimit)) {
                i = j + 1
                message("next next")
                next
              }

              j = j + 1
              y = MainTime[j]
            }
          } else {
            j=j+1
          }

          # At this point. this should always be true
          # Step 3 and 4, processing should happen at this stage
          if (interval(x, y) < seconds(UpLimit) & interval(x, y) > seconds(LowLimit)) {

            TimeInt <- interval(x, y)

            progress$set(message = paste("Processing obs :",n, TimeInt), value = n/(length(MainTime)/4))

            message(paste("Processing obs :",n, TimeInt))

            Select <- MainLog()[(MainLog()$DateTime %within% TimeInt), ]

            # Create metadata for the selected L1a point

            Obs$MetadataL1b <- gen_metadataL1b(Select = Select)

            Obs$Metadata <- gen_metadata(Select = Select)

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

            if (any(str_detect(L1a$InstrumentList(), "HOCR"))) {

              UpdateProgress <- function(value = NULL, message = NULL, detail = NULL) {
                if (is.null(value)) {
                  value <- progress$getValue()
                  value <- value + (progress$getMax() - value) / 5
                }
                progress$set(value = value, message = message, detail = detail)
              }

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
                  mutate(DarkAproxData = purrr::map(AproxData, ~ .x[which.min(abs(.x$DateTime - ObsTime)), ])) %>%
                  ungroup() %>%
                  select(SN, DarkAproxData)

                Obs$HOCR$L1b <- spsComps::shinyCatch(
                  cal_hocr(
                    RawHOCR = FiltRawHOCR,
                    CalHOCR = CalData$CalHOCR(),
                    HOCRDark = HOCRDark,
                    MainLogDate = unique(date(Select$DateTime)),
                    UpdateProgress
                  ),
                  shiny = T,
                  trace_back = TRUE
                )

                # L2

                WaveSeq <- seq(
                  Settings$HOCR$WaveMin(),
                  Settings$HOCR$WaveMax(),
                  Settings$HOCR$WaveStep()
                )

                Z1Depth <- Settings$HOCR$Z1Depth()
                Z1Z2Depth <- Settings$HOCR$Z1Z2Depth()

                Obs$HOCR$L2 <- tryCatch({
                  L2_hocr(Obs$HOCR$L1b, WaveSeq, Z1Depth, Z1Z2Depth,
                          T, 0.1, Obs)
                },
                error = function(e) e
                )

                if (inherits(Obs$HOCR$L2, "error")) {
                  #message(Obs$HOCR$L2)
                  message("Sear is trying to take the next j point")

                  # Delete the outdated metadata initially created
                  #  Necessarry to respect UUID foreign key rule (present in main table)
                  #DBI::dbWriteTable(DB$Con(), paste0("DELETE * FROM Metadata WHERE UUID = ",ObsUUID,";"))

                  i = i
                  j = j + 1

                  next
                }

                ObsUUID <- uuid::UUIDgenerate(
                  use.time = T,
                  output = "string"
                )

                Metadata <- Obs$Metadata %>%
                  mutate(
                    UUID = ObsUUID,
                    ProTime = as.character(as.POSIXlt(Sys.time(), tz = "UTC")),
                    Analyst = "Raphael Mabit",
                    Mail = "raphael.mabit@gmail.com"
                  )

                DBI::dbWriteTable(DB$Con(), "Metadata", Metadata, append = TRUE)

                MetadataL1b <- Obs$MetadataL1b %>%
                  mutate(
                    UUID = ObsUUID
                  )

                DBI::dbWriteTable(DB$Con(), "MetadataL1b", MetadataL1b, append = TRUE)

                # HOCR

                HOCRL1b <- Obs$HOCR$L1b %>%
                  unnest(cols = c(AproxData)) %>%
                  mutate(UUID = ObsUUID)

                HOCRL2 <- Obs$HOCR$L2 %>%
                  mutate(UUID = ObsUUID)

                DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
                DBI::dbWriteTable(DB$Con(), "HOCRL2", HOCRL2, append = TRUE)

              }
            }

            # SBE19 L1b ---------------------------------------------------------------

            if (any(str_detect(L1a$InstrumentList(), "SBE19"))) {

              progress$set(message = "Processing L1b: ", value = progress$getValue())

              progress$set(value = 0.3, detail = "SBE19")

              Lon <- mean(Select$Lon)
              Lat <- mean(Select$Lat)

              SBE19 <- L1a$SBE19() %>% filter(DateTime %within% TimeInt)

              if (nrow(SBE19) == 0) {
                warning(
                  paste0("SBE19 data not found at time interval: ",TimeInt)
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
                  select(!any_of(c("Conductivity", "CT", "O2Sol")))%>%
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

                SBE19L2 <- Obs$SBE19$L2  %>%
                  mutate(UUID = ObsUUID)

                DBI::dbWriteTable(DB$Con(), "SBE19L1b", SBE19L1b, append = TRUE)
                DBI::dbWriteTable(DB$Con(), "SBE19L2", SBE19L2, append = TRUE)

              }
            }

            # SeaOWL L1b --------------------------------------------------------------

            if (any(str_detect(L1a$InstrumentList(), "SeaOWL"))) {

              progress$set(value = 0.4, detail = "SeaOWL")

              SeaOWL <- L1a$SeaOWL() %>% filter(DateTime %within% TimeInt)

              if (nrow(SeaOWL) == 0) {
                warning(
                  paste0("SeaOWL data not found at time interval: ",TimeInt)
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
                  select(!any_of(c("SN")))%>%
                  pivot_longer(
                    cols = any_of(c("Bb_700", "Chl", "FDOM")),
                    names_to = "Parameter",
                    values_to = "Value"
                  ) %>%
                  group_by(Parameter) %>%
                  nest(Data = !matches("Parameter"))

                Obs$SeaOWL$L2 <- L2_param_val(Obs$SeaOWL$L1b)

                SeaOWLL1b <- Obs$SeaOWL$L1b %>%
                  unnest(c(Data)) %>%
                  mutate(UUID = ObsUUID)

                SeaOWLL2 <- Obs$SeaOWL$L2  %>%
                  mutate(UUID = ObsUUID)

                DBI::dbWriteTable(DB$Con(), "SeaOWLL1b", SeaOWLL1b, append = TRUE)
                DBI::dbWriteTable(DB$Con(), "SeaOWLL2", SeaOWLL2, append = TRUE)

              }
            }

            # BBFL2 L1b ---------------------------------------------------------------

            if (any(str_detect(L1a$InstrumentList(), "BBFL2"))) {

              progress$set(value = 0.5, detail = "BBFL2")

              BBFL2 <- L1a$BBFL2() %>% filter(DateTime %within% TimeInt)

              if (nrow(BBFL2) == 0) {
                warning(
                  paste0("BBFL2 data not found at time interval: ",TimeInt)
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

                BBFL2L2 <- Obs$BBFL2$L2  %>%
                  mutate(UUID = ObsUUID)

                DBI::dbWriteTable(DB$Con(), "BBFL2L1b", BBFL2L1b, append = TRUE)
                DBI::dbWriteTable(DB$Con(), "BBFL2L2", BBFL2L2, append = TRUE)

              }
            }

            # BioSonic L1b ---------------------------------------------------------------

            if (any(str_detect(L1a$InstrumentList(), "BioSonic"))) {

              progress$set(value = 0.6, detail = "BioSonic")

              BioSonicL1b <- L1a$BioSonic() %>% filter(DateTime %within% TimeInt)

              if (nrow(BioSonicL1b) == 0) {
                warning(
                  paste0("BioSonic data not found at time interval: ",TimeInt)
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

                BioSonicL1b <- Obs$BioSonic$L1b  %>%
                  mutate(UUID = ObsUUID)

                BioSonicL2 <- Obs$BioSonic$L2  %>%
                  mutate(UUID = ObsUUID)

                DBI::dbWriteTable(DB$Con(), "BioSonicL1b", BioSonicL1b, append = TRUE)
                DBI::dbWriteTable(DB$Con(), "BioSonicL2", BioSonicL2, append = TRUE)

              }
            }

            # Feedback to the user
            # session$sendCustomMessage(
            #   type = "testmessage",
            #   message = "Saved"
            #   # glue::glue(
            #   #   "Metadata: ",MetaUp," entry updated\n",
            #   #   "HOCRL1b: ", sum(L1bUp)," entry updated\n",
            #   #   "HOCRL2: ",L2Up," entry updated\n")
            # )

            # qry <- glue::glue_sql("SELECT * FROM Metadata WHERE UUID = '", ObsUUID, "';")
            #
            # Obs$Metadata <- tibble(DBI::dbGetQuery(DB$Con(), qry))

            n = n+1
          }

          i = j
        }

        # Update the list of observation
        DB$ObsMeta(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM Metadata")))

      })


  # Module output -----------------------------------------------------------
  list(
  )


  })
}

## To be copied in the UI
# mod_automatic_processing_ui("automatic_processing")

## To be copied in the server
# mod_automatic_processing_server("automatic_processing")
