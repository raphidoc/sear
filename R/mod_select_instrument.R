#' select_instrument UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("InstrumentList"))
  )
}

#' select_instrument Server Functions
#'
#' @noRd
mod_select_instrument_server <- function(id, MainLog) {
  stopifnot(is.reactive(MainLog))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    InstrumentList <- reactive({

      InstrumentList <- MainLog() %>%
        select(Instrument) %>%
        unique() %>%
        filter(Instrument %in% c("OCR1", "OCR2", "OCR3", "CTD", "ECO", "OWL"))

      InstrumentList <- InstrumentList[[1]]

      OCRList <- InstrumentList[str_detect(InstrumentList, "OCR")]

      if (any(OCRList %in% c("OCR1", "OCR2", "OCR3")) & any(!OCRList %in% c("OCR1", "OCR2", "OCR3"))) {
        MissingOCR <- c("OCR1", "OCR2", "OCR3")[which(!c("OCR1", "OCR2", "OCR3") %in% InstrumentList)]

        warning("HOCR on port ", MissingOCR, "is missing from MainLog. Cannot process HOCR data.")

        InstrumentList <- str_remove(InstrumentList, "OCR1|OCR2|OCR3")
      }

      InstrumentList <- case_when(
        str_detect(InstrumentList, "OCR1|OCR2|OCR3") ~ "HOCR",
        str_detect(InstrumentList, "CTD") ~ "SBE19",
        str_detect(InstrumentList, "ECO") ~ "BBFL2",
        str_detect(InstrumentList, "OWL") ~ "SeaOWL"
      ) %>%
        unique()
    })

    output$InstrumentList <- renderUI({
      req(MainLog)

      checkboxGroupInput(
        ns("InstrumentList"),
        "Intrument to process",
        choices = InstrumentList(),
        selected = InstrumentList(),
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      )
    })

    list(
      ToProcess = reactive({
        input$InstrumentList
      })
    )
  })
}

## To be copied in the UI
# mod_select_instrument_ui("select_instrument_1")

## To be copied in the server
# mod_select_instrument_server("select_instrument_1")
