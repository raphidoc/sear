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
    uiOutput(outputId = ns("L1b"))
  )
}

#' process_L1L2 Server Functions
#'
#' @noRd
mod_L1b_process_server <- function(id, L1, SelData, CalData, Obs) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$L1b <- renderUI({
      req(L1$Apla())

      tagList(
        waiter::use_waiter(),
        #mod_select_instrument_ui(ns("select_instrument")),
        actionButton(ns("ProcessL1b"), "ProcessL1b")
      )
    })

    #Instrument <- mod_select_instrument_server("select_instrument", L1$MainLog)

    observeEvent(
      input$ProcessL1b,
      label = "processL1b",
      {
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())

        if (is.null(Instrument$ToProcess())) {
          showModal(modalDialog(
            title = "No instrument selected",
            "Please select at least one instrument to process"
          ))
          invalidateLater(1)
        }

        # Filter data point before processing to optimize execution time
        SelDateTime <- SelData$Apla()$DateTime[SelData$Apla()$ID %in% SelData$SelApla()$ID]
        TimeInt <- interval(min(SelDateTime, na.rm = T), max(SelDateTime, na.rm = T))

        # HOCR L1b ----------------------------------------------------------------

        if (any(str_detect(Instrument$ToProcess(), "HOCR"))) {

          FiltRawHOCR <- filter_hocr(L1$HOCR(), L1$HOCRTimeIndex(), TimeInt)

          # Select nearest dark data
          ObsTime <- int_end(TimeInt / 2)

          HOCRDark <- L1$HOCRDark() %>%
            mutate(DarkAproxData = purrr::map(AproxData, ~ .x[which.min(abs(.x$DateTime - ObsTime)), ])) %>%
            ungroup() %>%
            select(SN, DarkAproxData)

          Obs$HOCR$L1b <- spsComps::shinyCatch(
            cal_hocr(
              RawHOCR = FiltRawHOCR,
              CalHOCR = CalData$CalHOCR(),
              HOCRDark = HOCRDark,
              AplaDate = unique(date(SelData$SelApla()$DateTime))
              ),
            shiny = T,
            trace_back = TRUE
          )
        }

        # SBE19 L1b ---------------------------------------------------------------

        if (any(str_detect(Instrument$ToProcess(), "SBE19"))) {

          Lon <- mean(SelData$SelApla()$Lon_DD)
          Lat <- mean(SelData$SelApla()$Lat_DD)

          CTD <- L1$SBE19() %>% filter(DateTime %within% TimeInt)

          CTD <- cal_sbe19(CTD, Lon, Lat) %>%
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
              ID = seq_along(rownames(CTD)),
              QC = "1"
            )

          Obs$SBE19$L1b <- CTD %>%
            select(!any_of(c("Conductivity", "CT", "O2Sol")))%>%
            pivot_longer(
              cols = any_of(c("Temperature", "Pressure", "SP", "SA", "OxSol", "Oxygen", "pH")),
              names_to = "Parameter",
              values_to = "Value"
            ) %>%
            group_by(Parameter) %>%
            nest(Data = !matches("Parameter"))

        }

        # SeaOWL L1b --------------------------------------------------------------

        if (any(str_detect(Instrument$ToProcess(), "SeaOWL"))) {

          SeaOWL <- L1$SeaOWL() %>% filter(DateTime %within% TimeInt)

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

        # BBFL2 L1b ---------------------------------------------------------------

        if (any(str_detect(Instrument$ToProcess(), "BBFL2"))) {

          BBFL2 <- L1$BBFL2() %>% filter(DateTime %within% TimeInt)

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

        # Empty L2 on new processing to avoid confusion
        Obs$HOCR$L2 <- tibble()
        Obs$SBE19$L2 <- tibble()
        Obs$SeaOWL$L2 <- tibble()
        Obs$BBFL2$L2 <- tibble()

      }
    )

# Module output -----------------------------------------------------------
    list(
      SelApla = SelData$SelApla,
      Map = SelData$Map,
      ProcessL1b = reactive(input$ProcessL1b)
    )
  })
}

## To be copied in the UI
# mod_L1b_process_ui("L1b_process")

## To be copied in the server
# mod_L1b_process_server("L1b_process")
