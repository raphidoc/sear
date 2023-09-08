#' parse_biosonic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_biosonic_ui <- function(id){
  ns <- NS(id)
  tagList(
    waiter::use_waiter(),
    uiOutput(outputId = ns("Load"))
  )
}

#' parse_biosonic Server Functions
#'
#' @noRd
mod_parse_biosonic_server <- function(id, SearTbl, ParsedFiles){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    BioSonic <- reactiveVal()

    output$Load <- renderUI({
      req(SearTbl())

      fileInput(ns("Files"), "Choose BioSonic .csv Files", accept = c(".csv"), multiple = T)

    })

    observeEvent(
      input$Files,
      {

        # Copy files in raw dir

        RawDir <- file.path(SearTbl()$ProjPath, "sear", "data", "raw")

        dir.create(RawDir, recursive = TRUE)

        Files <- input$Files %>%
          mutate(
            rawpath = file.path(RawDir, name)
          )

        file.copy(Files$datapath, Files$rawpath)

        DateRange <- str_extract(Files$name, "[:digit:]{8}_[:digit:]{8}")

        ParsedDir <- file.path(SearTbl()$ProjPath, "sear", "data", "parsed")

        dir.create(ParsedDir, recursive = TRUE)

        BioSonic(read_biosonic(Files$rawpath))

        PotBioSonic <- file.path(ParsedDir, paste0("biosonic_",DateRange,".csv"))

        write_csv(BioSonic(), PotBioSonic)

      }
    )

    observeEvent(
      SearTbl(),
      {

        #NameBioSonic <- c("biosonic_[:digit:]{8}_[:digit:]{8}\\.csv")

        NameBioSonic <- c("biosonic.*\\.csv")

        if (any(str_detect(ParsedFiles(), NameBioSonic))) {

          PotBioSonic <- str_subset(ParsedFiles(), NameBioSonic)

          BioSonic(read_csv(PotBioSonic))

        }
      }
    )


# Module output -----------------------------------------------------------

    list(
      BioSonic = BioSonic,
      Input = reactive(input$Files)
    )

  })
}

## To be copied in the UI
# mod_parse_biosonic_ui("parse_biosonic")

## To be copied in the server
# mod_parse_biosonic_server("parse_biosonic")
