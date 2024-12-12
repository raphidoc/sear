#' select_instrument UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_instrument_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        uiOutput(ns("instrumentList"))
      ),
      column(
        width = 6,
        textOutput(ns("FilesPath"))
      )
    )
  )
}

#' select_instrument Server Functions
#'
#' @noRd
mod_select_instrument_server <- function(id, ParsedFiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    instrumentList <- reactive({
      instrumentList <- ParsedFiles() %>%
        str_extract("apla|bbfl2|hocr|sbe19|seaowl|biosonic|hb_devices")

      case_when(
        str_detect(instrumentList, "hocr") ~ "HOCR",
        str_detect(instrumentList, "sbe19") ~ "SBE19",
        str_detect(instrumentList, "bbfl2") ~ "BBFL2",
        str_detect(instrumentList, "seaowl") ~ "SeaOWL",
        str_detect(instrumentList, "biosonic") ~ "BioSonic",
        str_detect(instrumentList, "hb_devices") ~ "HydroBall"
      ) %>%
        unique() %>%
        na.omit()
    })

    output$instrumentList <- renderUI({
      req(ParsedFiles())

      checkboxGroupInput(
        ns("instrumentList"),
        "Intrument to process",
        choices = instrumentList(),
        selected = instrumentList(),
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      )
    })

    output$FilesPath <- renderText({
      basename(ParsedFiles())
    })

    list(
      instrumentList = reactive({
        input$instrumentList
      })
    )
  })
}

## To be copied in the UI
# mod_select_instrument_ui("select_instrument")

## To be copied in the server
# mod_select_instrument_server("select_instrument")
