#' parse_biosonic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_biosonic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("Load"))
  )
}

#' parse_biosonic Server Functions
#'
#' @noRd
mod_parse_biosonic_server <- function(id, SearProj, ParsedFiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Load <- renderUI({
      req(SearProj())

      fileInput(ns("Files"), "Choose BioSonic .csv Files", accept = c(".csv"), multiple = T)
    })

    observeEvent(
      input$Files,
      {
        RawDir <- file.path(SearProj()$ProjPath, "sear", "data", "raw")

        dir.create(RawDir, recursive = TRUE)

        Files <- input$Files %>%
          mutate(
            rawpath = file.path(RawDir, name)
          )

        file.copy(Files$datapath, Files$rawpath)

        DateRange <- str_extract(Files$name, "[:digit:]{8}_[:digit:]{8}")

        ParsedDir <- file.path(SearProj()$ProjPath, "sear", "data", "parsed")

        dir.create(ParsedDir, recursive = TRUE)

        BioSonic <- read_biosonic(Files$rawpath)

        PotBioSonic <- file.path(ParsedDir, paste0("biosonic_", DateRange, ".csv"))

        write_csv(BioSonic, PotBioSonic)
      }
    )

    # Module output -----------------------------------------------------------

    return(reactive(input$Files))
  })
}

## To be copied in the UI
# mod_parse_biosonic_ui("parse_biosonic")

## To be copied in the server
# mod_parse_biosonic_server("parse_biosonic")
