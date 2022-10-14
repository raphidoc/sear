#' process_L1L2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_process_L1b_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(outputId = ns("L1b"))

  )

}

#' process_L1L2 Server Functions
#'
#' @noRd
mod_process_L1b_server <- function(id, L1, SelData, CalData, Obs){

  # stopifnot(is.reactive(UpApla))
  # stopifnot(is.reactive(SelID))
  # stopifnot(is.reactive(RawHOCR))
  # stopifnot(is.reactive(TimeIndexHOCR))
  # stopifnot(is.reactive(CalData))

  moduleServer( id, function(input, output, session){
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

      if (is.null(Instrument$ToProcess())) {

        showModal(modalDialog(
          title = "No instrument selected",
          "Please select at least one instrument to process")
        )
        invalidateLater(1)

      }

      if (str_detect(Instrument$ToProcess(), "HOCR")) {

        # Filter data point before processing to optimize execution time

        SelDateTime <- SelData$Apla()$DateTime[SelData$Apla()$ID %in% SelData$SelApla()$ID]
        TimeInt <- interval(min(SelDateTime, na.rm = T), max(SelDateTime, na.rm = T))

        FiltRawHOCR <- filter_hocr(L1$HOCR(), L1$TimeIndexHOCR(), TimeInt)

        Obs$HOCR$L1b <- spsComps::shinyCatch(
          cal_hocr(FiltRawHOCR = FiltRawHOCR, CalHOCR = CalData()$HOCR, AplaDate = unique(date(SelData$SelApla()$DateTime))),
          trace_back = TRUE
        )

      }

      # # Create a temporary copy of the current Apla tibble
      # tmp <- Apla()
      #
      # # Update values in place
      # tmp$ObsType[tmp$ID %in% SelID()] <- input$ObsType
      # tmp$ObsName[tmp$ID %in% SelID()] <- input$ObsName
      #
      # # Update the Apla reactiveVal
      # Apla(tmp)
    })

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
