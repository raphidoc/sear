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
    uiOutput(outputId = ns("L1b")),
  )
}

#' process_L1L2 Server Functions
#'
#' @noRd
mod_L1b_process_server <- function(id, L1, SelData, CalData, Obs) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$L1b <- renderUI({

      req(L1$ParsedFiles())

      tagList(
        waiter::use_waiter(),
        #mod_select_instrument_ui(ns("select_instrument")),
        actionButton(ns("ProcessL1b"), "ProcessL1b")
      )
    })

    #Instrument <- mod_select_instrument_server("select_instrument", L1)

    observeEvent(
      input$ProcessL1b,
      label = "processL1b",
      {
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())

        if (is.null(L1$InstrumentList())) {
          showModal(modalDialog(
            title = "No instrument selected",
            "Please select at least one instrument to process"
          ))
          invalidateLater(1)
        }

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

        # Filter data point before processing to optimize execution time
        SelDateTime <- SelData$MainLog()$DateTime[SelData$MainLog()$ID %in% SelData$SelMainLog()$ID]
        TimeInt <- interval(min(SelDateTime, na.rm = T), max(SelDateTime, na.rm = T))

        # HOCR L1b ----------------------------------------------------------------

        if (any(str_detect(L1$InstrumentList(), "HOCR"))) {

          browser()

          FiltRawHOCR <- filter_hocr(L1$HOCR(), L1$HOCRTimeIndex(), TimeInt)

          if (length(FiltRawHOCR) == 0) {
            warning(
              paste0("HOCR data not found at time interval: ",TimeInt)
            )
          } else {

            # Select nearest dark data
            ObsTime <- int_end(TimeInt / 2)

            ### CHECK FOR BUG IN HOCRDark LOADING ###

            HOCRDark <- L1$HOCRDark() %>%
              mutate(DarkAproxData = purrr::map(AproxData, ~ .x[which.min(abs(.x$DateTime - ObsTime)), ])) %>%
              ungroup() %>%
              select(SN, DarkAproxData)

            Obs$HOCR$L1b <- spsComps::shinyCatch(
              cal_hocr(
                RawHOCR = FiltRawHOCR,
                CalHOCR = CalData$CalHOCR(),
                HOCRDark = HOCRDark,
                MainLogDate = unique(date(SelData$SelMainLog()$DateTime))
              ),
              shiny = T,
              trace_back = TRUE
            )
          }
        }

        # SBE19 L1b ---------------------------------------------------------------

        if (any(str_detect(L1$InstrumentList(), "SBE19"))) {

          Lon <- mean(SelData$SelMainLog()$Lon_DD)
          Lat <- mean(SelData$SelMainLog()$Lat_DD)

          SBE19 <- L1$SBE19() %>% filter(DateTime %within% TimeInt)

          if (nrow(SBE19) == 0) {
            warning(
              paste0("SBE19 data not found at time interval: ",TimeInt)
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
          }

        }

        # SeaOWL L1b --------------------------------------------------------------

        if (any(str_detect(L1$InstrumentList(), "SeaOWL"))) {

          SeaOWL <- L1$SeaOWL() %>% filter(DateTime %within% TimeInt)

          if (nrow(SeaOWL) == 0) {
            warning(
              paste0("SeaOWL data not found at time interval: ",TimeInt)
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
          }
        }

        # BBFL2 L1b ---------------------------------------------------------------

        if (any(str_detect(L1$InstrumentList(), "BBFL2"))) {

          BBFL2 <- L1$BBFL2() %>% filter(DateTime %within% TimeInt)

          if (nrow(BBFL2) == 0) {
            warning(
              paste0("BBFL2 data not found at time interval: ",TimeInt)
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

        if (any(str_detect(L1$InstrumentList(), "BioSonic"))) {

          BioSonicL1b <- L1$BioSonic() %>% filter(DateTime %within% TimeInt)

          if (nrow(BioSonicL1b) == 0) {
            warning(
              paste0("BioSonic data not found at time interval: ",TimeInt)
              )
          } else {

            Obs$BioSonic$L1b <- BioSonicL1b %>%
              mutate(
                ID = seq_along(rownames(BioSonicL1b)),
                QC = "1"
              )

          }
        }

      }
    )

# Module output -----------------------------------------------------------
    list(
      SelMainLog = SelData$SelMainLog,
      Map = SelData$Map,
      ProcessL1b = reactive(input$ProcessL1b)
    )
  })
}

## To be copied in the UI
# mod_L1b_process_ui("L1b_process")

## To be copied in the server
# mod_L1b_process_server("L1b_process")
