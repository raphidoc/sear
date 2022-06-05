#' process_L1L2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_process_L1b_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(outputId = ns("L1b"))

  )

}

#' process_L1L2 Server Functions
#'
#' @noRd
mod_process_L1b_server <- function(id, SelData, L1, CalData){

  # stopifnot(is.reactive(UpApla))
  # stopifnot(is.reactive(SelID))
  # stopifnot(is.reactive(RawHOCR))
  # stopifnot(is.reactive(TimeIndexHOCR))
  # stopifnot(is.reactive(CalData))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$L1b <- renderUI({

      tagList(
        waiter::use_waiter(),
        fluidRow(
          column(6,
                 selectInput(ns("ObsType"), "ObsType", choices = list("Station","Transect"), selected = NULL, multiple = F)
                 ),
          column(6,
                 textInput(ns("ObsName"), "ObsName", value = NA, placeholder = "Prefix")
                 )
        ),
        mod_select_instrument_ui(ns("select_instrument")),
        actionButton(ns("ProcessL1b"), "ProcessL1b")
      )
    })

    mod_select_instrument_server(ns("select_instrument"), L1$MainLog)

    Data <- eventReactive(input$ProcessL1b, {

      # Nice display to indicate that processing is happening
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())

      # # Create a temporary copy of the current UpApla tibble
      # tmp <- UpApla()
      #
      # # Update values in place
      # tmp$ObsType[tmp$ID %in% SelID()] <- input$ObsType
      # tmp$ObsName[tmp$ID %in% SelID()] <- input$ObsName
      #
      # # Update the UpApla reactiveVal
      # UpApla(tmp)

      # Filter data point before processing to optimize execution time
      SelDateTime <- SelData$UpApla()$DateTime[SelData$UpApla()$ID %in% SelData$SelApla()$ID]
      TimeInt <- interval(min(SelDateTime, na.rm = T), max(SelDateTime, na.rm = T))

      FiltRawHOCR <- filter_hocr(L1$HOCR(), L1$TimeIndexHOCR(), TimeInt)

      Data <- cal_hocr(FiltRawHOCR = FiltRawHOCR, CalHOCR = CalData()$HOCR, AplaDate = unique(date(SelData$SelApla()$DateTime)))

    })

    list(
      Data = Data,
      ObsType = reactive(input$ObsType),
      ObsName = reactive(input$ObsName),
      SelApla = SelData$SelApla,
      ProcessL1b = reactive(input$ProcessL1b)
      )

  })
}

## To be copied in the UI
# mod_process_L1L2_ui("process_L1L2")

## To be copied in the server
# mod_process_L1L2_server("process_L1L2")
