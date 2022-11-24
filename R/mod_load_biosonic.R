#' load_biosonic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_biosonic_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(outputId = ns("Load"))

  )
}

#' load_biosonic Server Functions
#'
#' @noRd
mod_load_biosonic_server <- function(id, SearTbl){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$Load <- renderUI({

      shinyFilesButton(ns("BioSonic"), label = "BioSonic", title = "Select BioSonic .csv", multiple = T)

    })

    shinyFileChoose(input, "BioSonic", root = c(root = "/"), filetypes = c("csv"))

    reactive({
      req(SearTbl())

      browser()

      L1Dir <- file.path(SearTbl()$ProjPath, "L1")

      if (dir.exists(L1Dir) && any(str_detect(list.files(L1Dir), "SBES.*\\.csv"))) {

        BioSonicFile <- str_subset(list.files(L1Dir, full.names = T), "SBES.*\\.csv")

      } else {

        validate(need(is.list(input$BioSonic), "No BioSonic file provided"))

        BioSonicInput <- parseFilePaths(roots = c(root = ""), input$BioSonic)

        dir.create(L1Dir)

        BioSonicFile <- file.path(L1Dir, BioSonicInput$name)

        file.copy(str_extract(BioSonicInput$datapath, "(?<=/).*"), BioSonicFile)
      }


    })

  })
}

## To be copied in the UI
# mod_load_biosonic_ui("load_biosonic")

## To be copied in the server
# mod_load_biosonic_server("load_biosonic")
