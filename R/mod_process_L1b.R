#' process_L1L2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_process_L1b_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("L1b"))
  )
}

#' process_L1L2 Server Functions
#'
#' @noRd
mod_process_L1b_server <- function(id, L1, SelData, CalData, Obs) {
  # stopifnot(is.reactive(UpApla))
  # stopifnot(is.reactive(SelID))
  # stopifnot(is.reactive(RawHOCR))
  # stopifnot(is.reactive(TimeIndexHOCR))
  # stopifnot(is.reactive(CalData))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$L1b <- renderUI({
      req(L1$Apla())

      tagList(
        waiter::use_waiter(),
        mod_select_instrument_ui(ns("select_instrument")),
        actionButton(ns("ProcessL1b"), "ProcessL1b")
      )
    })

    Instrument <- mod_select_instrument_server("select_instrument", L1$MainLog)

    observeEvent(
      input$ProcessL1b,
      label = "processL1b",
      {
        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())

        browser()

        if (is.null(Instrument$ToProcess())) {
          showModal(modalDialog(
            title = "No instrument selected",
            "Please select at least one instrument to process"
          ))
          invalidateLater(1)
        }

        if (any(str_detect(Instrument$ToProcess(), "HOCR"))) {
          # Filter data point before processing to optimize execution time

          SelDateTime <- SelData$Apla()$DateTime[SelData$Apla()$ID %in% SelData$SelApla()$ID]
          TimeInt <- interval(min(SelDateTime, na.rm = T), max(SelDateTime, na.rm = T))

          FiltRawHOCR <- filter_hocr(L1$HOCR(), L1$TimeIndexHOCR(), TimeInt)

          # Select nearest dark data
          ObsTime <- int_end(TimeInt / 2)

          DarkHOCR <- L1$DarkHOCR() %>%
            mutate(DarkAproxData = purrr::map(AproxData, ~ .x[which.min(abs(.x$DateTime - ObsTime)), ])) %>%
            ungroup() %>%
            select(SN, DarkAproxData)

          Obs$HOCR$L1b <- spsComps::shinyCatch(
            cal_hocr(RawHOCR = FiltRawHOCR, CalHOCR = CalData()$HOCR, DarkHOCR = DarkHOCR, AplaDate = unique(date(SelData$SelApla()$DateTime))),
            shiny = T,
            trace_back = TRUE
          )

          # Empty L2 to avoid confusion
          Obs$HOCR$L2 <- tibble()
        }

        if (any(str_detect(Instrument$ToProcess(), "SBE19"))) {
          L1$SBE19()
        }

        if (any(str_detect(Instrument$ToProcess(), "BBFL2"))) {
          L1$BBFL2()
        }

        if (any(str_detect(Instrument$ToProcess(), "SeaOWL"))) {
          L1$SeaOWL()
        }
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
# mod_process_L1L2_ui("process_L1L2")

## To be copied in the server
# mod_process_L1L2_server("process_L1L2")
