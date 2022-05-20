#' load_mtelog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_mtelog_ui <- function(id){
  ns <- NS(id)
  tagList(

    #fileInput(ns("mtelog"), NULL, buttonLabel = "MTE", multiple = TRUE, accept = c(".txt",".bin"))
    uiOutput(outputId = ns("LoadButton"))

  )
}

#' load_mtelog Server Functions
#'
#' @noRd
mod_load_mtelog_server <- function(id, SearTbl){

  stopifnot(is.reactive(SearTbl))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$LoadButton <- renderUI({
      req(SearTbl)

      shinyFilesButton(ns('MTEfiles'), label="MTEfiles", title='Select MTE .txt and .bin files', multiple=T)
    })

    shinyFileChoose(input, 'MTEfiles', root=c(root='/'), filetypes=c('bin', 'txt'))

    reactive({
      req(SearTbl())

      MTEbulkLoc <- file.path(SearTbl()$ProjPath, "bulk")

      if (dir.exists(MTEbulkLoc) && length(list.files(MTEbulkLoc)) >= 2) {

        MTEbulkFiles <- list.files(MTEbulkLoc, full.names = T)

      } else {

        validate(need(is.list(input$MTEfiles), "No MTE files provided"))

        MTEinput <- parseFilePaths(roots = c(root=''), input$MTEfiles)

        dir.create(MTEbulkLoc)

        MTEbulkFiles <- file.path(MTEbulkLoc, MTEinput$name)

        file.copy(str_extract(MTEinput$datapath, "(?<=/).*"), MTEbulkFiles)

      }

      reactiveValues(
        txt = {stringr::str_subset(MTEbulkFiles, ".txt")},
        bin = {stringr::str_subset(MTEbulkFiles, ".bin")}
      )

    })
  })
}

## To be copied in the UI
# mod_load_mtelog_ui("load_mtelog_1")

## To be copied in the server
# mod_load_mtelog_server("load_mtelog_1")
