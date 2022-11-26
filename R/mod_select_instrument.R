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
mod_select_instrument_server <- function(id, ParsedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    InstrumentList <- reactive({

      InstrumentList <- ParsedFiles() %>%
        str_extract("apla|bbfl2|hocr|sbe19|seaowl|biosonic")

      case_when(
        str_detect(InstrumentList, "hocr") ~ "HOCR",
        str_detect(InstrumentList, "sbe19") ~ "SBE19",
        str_detect(InstrumentList, "bbfl2") ~ "BBFL2",
        str_detect(InstrumentList, "seaowl") ~ "SeaOWL",
        str_detect(InstrumentList, "biosonic") ~ "BioSonic"
      ) %>%
        unique() %>%
        na.omit()
    })

    output$InstrumentList <- renderUI({
      req(ParsedFiles())

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
      ToProcess = reactive({input$InstrumentList})
    )
  })
}

## To be copied in the UI
# mod_select_instrument_ui("select_instrument")

## To be copied in the server
# mod_select_instrument_server("select_instrument")
