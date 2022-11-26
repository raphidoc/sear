#' load_mtelog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_mtelog_ui <- function(id) {
  ns <- NS(id)
  tagList(

    uiOutput(outputId = ns("Load"))

  )
}

#' load_mtelog Server Functions
#'
#' @noRd
mod_load_mtelog_server <- function(id, SearTbl) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Load <- renderUI({
      req(SearTbl())

      fileInput("Files", "Choose MTE txt and bin Files", accept = c(".txt",".bin"), multiple = T)

    })


    reactive({

      req(input$Files)

      reactiveValues(
            txt = {
              stringr::str_subset(input$Files, ".txt")
            },
            bin = {
              stringr::str_subset(input$Files, ".bin")
            }
      )

    })


    # reactive({
    #   req(SearTbl())
    #
    #   MTEbulkLoc <- file.path(SearTbl()$ProjPath, "L1")
    #
    #   if (dir.exists(MTEbulkLoc) && length(list.files(MTEbulkLoc)) >= 2) {
    #     MTEbulkFiles <- list.files(MTEbulkLoc, full.names = T)
    #   } else {
    #     validate(need(is.list(input$MTEfiles), "No MTE files provided"))
    #
    #     MTEinput <- parseFilePaths(roots = c(root = ""), input$MTEfiles)
    #
    #     dir.create(MTEbulkLoc)
    #
    #     MTEbulkFiles <- file.path(MTEbulkLoc, MTEinput$name)
    #
    #     file.copy(str_extract(MTEinput$datapath, "(?<=/).*"), MTEbulkFiles)
    #   }
    #
    #   reactiveValues(
    #     txt = {
    #       stringr::str_subset(MTEbulkFiles, ".txt")
    #     },
    #     bin = {
    #       stringr::str_subset(MTEbulkFiles, ".bin")
    #     }
    #   )
    # })
  })
}

## To be copied in the UI
# mod_load_mtelog_ui("load_mtelog")

## To be copied in the server
# mod_load_mtelog_server("load_mtelog")
