#' parse_mtelog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_mtelog_ui <- function(id){
  ns <- NS(id)
  tagList(

    waiter::use_waiter()

  )
}

#' parse_mtelog Server Functions
#'
#' @noRd
mod_parse_mtelog_server <- function(id, DataFiles){

  stopifnot(is.reactive(DataFiles))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # DataLogger reactive tibble ----------------------------------------------

    MainLog <- reactive(label = "MainLog",{
      req(DataFiles())

      read_mtelog(DataFiles()$txt)

    })

    # Aplanix data ------------------------------------------------------------

    Apla <- reactiveVal({})

    # Initial tibble created on datalogger file upload
    observe({
      Apla(read_apla(MainLog()))
    })

    # HOCR binary read --------------------------------------------------------

    HOCR <- reactive({
      req(DataFiles())

      read_hocr(DataFiles()$bin)
    })

    TimeIndexHOCR <- reactive({
      req(Apla())

      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())

      # Dont know the logger date format so quick and dirty fix with Apla date
      AplaDate <- unique(date(Apla()$DateTime))

      # Posixct object appear to be heavy, same length list of DateTime is heavier (25.8 MB) than the list of HOCR packets (22.2)
      # Computation time arround 2/3 minutes
      purrr::map(.x = HOCR(), ~ clock::date_time_parse(paste0(AplaDate," ",hms::as_hms(.x$gpstime/1000)), zone = "UTC"))
    })

    # Force TimeIndexHOCR computation on loading of data (repartition of waiting time...)
    observe({
      TimeIndexHOCR()
    })


    # filter MTE bulk data to L1 ----------------------------------------------
    # observe({
    #   req(DataFiles())
    #
    #   Apla(Apla() %>% filter(Speed_N < 4))
    #
    # })


    list(
      MainLog = MainLog,
      Apla = Apla,
      HOCR = HOCR,
      TimeIndexHOCR = TimeIndexHOCR
      )

  })
}

## To be copied in the UI
# mod_parse_mtelog_ui("parse_mtelog_1")

## To be copied in the server
# mod_parse_mtelog_server("parse_mtelog_1")
