#' load_cal_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_cal_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("Load"))
  )
}

#' load_cal Server Functions
#'
#' @noRd
mod_load_cal_server <- function(
    id,
    SearTbl,
    read_cal,
    ReactCal,
    ParsedCalFiles
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$Load <- renderUI({
      req(SearTbl())

      fileInput(ns("Cal"), paste0("Select ",id," calibration files"), accept = c(".cal"), multiple = T)

    })

    observeEvent(
      input$Cal,
      {

        CalDir <- file.path(SearTbl()$ProjPath, ".sear", "cal")

        dir.create(CalDir, recursive = TRUE)

        Files <- input$Cal %>%
          mutate(
            calpath = file.path(CalDir, paste0(id,"_",name))
          )

        file.copy(Files$datapath, Files$calpath)

        Cal <- read_cal(Files$calpath)

        ReactCal(Cal)

      }
    )

    observeEvent(
      ignoreInit = F,
      ParsedCalFiles(),
      {

        if (any(str_detect(ParsedCalFiles(), id))) {

          Pot <- str_subset(ParsedCalFiles(), id)

          temp <- read_cal(Pot)

          ReactCal(temp)
        }

      }
    )

  })
}

## To be copied in the UI
# mod_load_cal_ui("load_cal")

## To be copied in the server
# mod_load_cal_server("load_cal")
