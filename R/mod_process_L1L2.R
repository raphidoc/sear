#' process_L1L2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_process_L1L2_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(outputId = ns("L1b"))

  )

}

#' process_L1L2 Server Functions
#'
#' @noRd
mod_process_L1L2_server <- function(id, Apla, UpApla, Selected, RawHOCR, TimeIndexHOCR, CalData, MainLog){

  stopifnot(is.reactive(Apla))
  stopifnot(is.reactive(UpApla))
  stopifnot(is.reactive(Selected))
  stopifnot(is.reactive(RawHOCR))
  stopifnot(is.reactive(TimeIndexHOCR))
  stopifnot(is.reactive(CalData))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$L1b <- renderUI({

      req(MainLog)

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
        actionButton(ns("ProcessL1b"), "ProcessL1b")
      )
    })

    Data <- eventReactive(input$ProcessL1b, {

      # Nice display to indicate that processing is happening
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())

      # Create a temporary copy of the current UpApla tibble
      tmp <- UpApla()

      # Update values in place
      tmp$ObsType[tmp$ID %in% Selected()] <- input$ObsType
      tmp$ObsName[tmp$ID %in% Selected()] <- input$ObsName

      # Update the UpApla reactiveVal
      UpApla(tmp)

      # Filter data point before processing to optimize execution time
      SelDateTime <- UpApla()$DateTime[UpApla()$ID %in% Selected()]
      TimeInt <- interval(min(SelDateTime, na.rm = T), max(SelDateTime, na.rm = T))

      FiltRawHOCR <- filter_hocr(RawHOCR(), TimeIndexHOCR(), TimeInt)

      Data <- process_station(FiltRawHOCR = FiltRawHOCR, CalData = CalData(), Apla = Apla())



      # Process selected data point according to ObsType
      # if (input$ObsType %in% c("Unkown","Transit")) {
      #   message("Nothing to process")
      # } else if (input$ObsType == "Station") {
      #
      #   FiltRawHOCR <- filter_hocr(RawHOCR(), TimeIndexHOCR(), TimeInt)
      #
      #   Station <- reactive({
      #     process_station(FiltRawHOCR = FiltRawHOCR, CalData = CalData(), Apla = Apla())
      #   })
      #
      #
      # } else if (input$ObsType == "Transect") {
      #
      #   #Transect <- process_transect(CalHOCR = CalHOCR())
      #
      # } else {
      #   error("ObsType has a problem ...")
      # }

      #invalidateLater(500)
    })

    ObsType <- reactive({
      input$ObsType
    })

    list(
      Data = Data,
      ObsType = ObsType,
      ProcessL1b = reactive(input$ProcessL1b)
      )


  })
}

## To be copied in the UI
# mod_process_L1L2_ui("process_L1L2")

## To be copied in the server
# mod_process_L1L2_server("process_L1L2")
