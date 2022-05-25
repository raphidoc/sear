#' parse_mtelog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import readr
mod_parse_mtelog_ui <- function(id){
  ns <- NS(id)
  tagList(

    waiter::use_waiter()

  )
}

#' parse_mtelog Server Functions
#'
#' @noRd
mod_parse_mtelog_server <- function(id, SearTbl, DataFiles){

  stopifnot(is.reactive(SearTbl))
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

    # Initial tibble
    observe({

      # Try to read parsed and filtered applanix data

      #Potential filtered File
      PotApla <- file.path(SearTbl()$ProjPath,"L1",paste0("filtered_apla_",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

      if (file.exists(PotApla)) {

        Apla(read_csv(PotApla))

      } else {

        validate(need(MainLog(), label = "Need Raw Applanix data to create mainlog"))

        Apla(read_apla(MainLog()))

        Apla(Apla() %>% filter(Speed_N <= 4))

        dir.create(file.path(SearTbl()$ProjPath,"L1"))

        write_csv(Apla(), PotApla)
      }

    })

    # HOCR binary read --------------------------------------------------------

    HOCR <- reactiveVal()
    TimeIndexHOCR <- reactiveVal()

      observe({
        req(DataFiles())

        waiter <- waiter::Waiter$new()
        waiter$show()
        on.exit(waiter$hide())

        PotHocr <- file.path(SearTbl()$ProjPath, "L1", paste0("filtered_hocr_",str_extract(DataFiles()$bin, "[[:digit:]]{8}_[[:digit:]]{6}"),".rds"))

        PotTimeIndexHocr <- file.path(SearTbl()$ProjPath, "L1",
                                      paste0("filtered_time_index_hocr_",str_extract(DataFiles()$bin, "[[:digit:]]{8}_[[:digit:]]{6}"),".rds"))

        if (file.exists(PotHocr) & file.exists(PotTimeIndexHocr)) {

          HOCR(read_rds(PotHocr))

          TimeIndexHOCR(read_rds(PotTimeIndexHocr))

        } else {

          validate(need(DataFiles()$bin, label = "Need raw HOCR"))

          Hocr <- read_hocr(DataFiles()$bin)

          # Dont know the logger date format so quick and dirty fix with Apla date
          AplaDate <- unique(date(Apla()$DateTime))

          # Posixct object appear to be heavy, same length list of DateTime is heavier (25.8 MB) than the list of HOCR packets (22.2)
          # Computation time arround 2/3 minutes
          TimeIndex <- purrr::map(.x = Hocr, ~ clock::date_time_parse(paste0(AplaDate," ",hms::as_hms(.x$gpstime/1000)), zone = "UTC"))

          # Apla()$DateTime is filtered for speed < 4 knt
          Hocr <- Hocr[TimeIndex %in% Apla()$DateTime]

          TimeIndex <- TimeIndex[TimeIndex %in% Apla()$DateTime]

          HOCR(Hocr)

          TimeIndexHOCR(TimeIndex)

          write_rds(Hocr, PotHocr)

          write_rds(TimeIndex, PotTimeIndexHocr)
        }


      })

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
