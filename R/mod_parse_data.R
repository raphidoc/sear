#' parse_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_select_instrument_ui(ns("select_instrument")),
    uiOutput(outputId = ns("TabPanel"))
  )
}

#' parse_data Server Functions
#'
#' @noRd
mod_parse_data_server <- function(id, SearTbl, CalData, Apla){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$TabPanel <- renderUI({
      req(SearTbl())

      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
        tabPanel(
          "SeaDooMTE",
          mod_parse_mtelog_ui(ns("parse_mtelog"))
        ),
        tabPanel(
          "BioSonic",
          mod_parse_biosonic_ui(ns("parse_biosonic"))
        )
      )
    })



    #DataFiles <- mod_load_data_server("load_data", SearTbl)

    ParsedFiles <- eventReactive(
      {
        c(
          SearTbl(),
          MTELog$Input(),
          BioSonic$Input()
          )
      },
      {

        ParsedDir <- file.path(SearTbl()$ProjPath, ".sear", "data", "parsed")

        if (dir.exists(ParsedDir)) {

          list.files(ParsedDir, full.names = TRUE)

        } else {
          FALSE
        }

      }
    )

    ToProcess <- mod_select_instrument_server("select_instrument", ParsedFiles)
    MTELog <- mod_parse_mtelog_server("parse_mtelog", SearTbl, CalData, Apla, ParsedFiles)
    BioSonic <- mod_parse_biosonic_server("parse_biosonic", SearTbl, ParsedFiles)

# Module output -----------------------------------------------------------

    append(
      list(
        ParsedFiles = ParsedFiles
      ),
      c(
        ToProcess,
        MTELog,
        BioSonic
      )
    )

  })
}

## To be copied in the UI
# mod_parse_data_ui("parse_data")

## To be copied in the server
# mod_parse_data_server("parse_data")
