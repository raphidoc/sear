#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(ns("WaveSeq")),
    uiOutput(ns("PositionHOCR"))

  )
}

#' settings Server Functions
#'
#' @noRd
mod_settings_server <- function(id, SearProj){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$WaveSeq <- renderUI({

      req(SearProj$History())

      tagList(
        numericInput(
          ns("WaveMin"),
          "Minimum Wavelength [nm]",
          353,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        ),
        numericInput(
          ns("WaveMax"),
          "Maximum Wavelength [nm]",
          800,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        ),
        numericInput(
          ns("WaveStep"),
          "Wavelength Step [nm]",
          3,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        )
      )

    })

    output$PositionHOCR <- renderUI({
      req(SearProj$History())

      tagList(
        numericInput(
          ns("Z1Depth"),
          "LuZ1Depth",
          NA,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        ),
        numericInput(
          ns("Z1Z2Depth"),
          "Depth difference LuZ1-LuZ2",
          NA,
          min = NA,
          max = NA,
          step = NA,
          width = NULL
        )
      )

    })


    LastSettings <- reactive({



    })

# Module output -----------------------------------------------------------

Settings <- reactiveValues(
  HOCR = reactiveValues(
    WaveMin = reactive(input$WaveMin),
    WaveMax = reactive(input$WaveMax),
    WaveStep = reactive(input$WaveStep),
    Z1Depth = reactive(input$Z1Depth),
    Z1Z2Depth = reactive(input$Z1Z2Depth),

  )
)

  })
}

## To be copied in the UI
# mod_settings_ui("settings")

## To be copied in the server
# mod_settings_server("settings")
