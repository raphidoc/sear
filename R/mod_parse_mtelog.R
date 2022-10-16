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
mod_parse_mtelog_server <- function(id, SearTbl, DataFiles, Apla, ECO){

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

    #Apla <- reactiveVal({})

    # Initial tibble
    observe({

      # Try to read parsed and filtered applanix data

      #Potential filtered File
      PotApla <- file.path(SearTbl()$ProjPath,".sear",paste0("filtered_apla_",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

      if (file.exists(PotApla)) {

        Apla(read_csv(PotApla))

      } else {

        validate(need(MainLog(), label = "Need Raw Applanix data to create mainlog"))

        Apla(read_apla(MainLog()))

        # Apla(Apla() %>% filter(
        #   Speed_N <= 4,
        #   BoatSolAzm > 0 & BoatSolAzm < 180
        #   ))

        dir.create(file.path(SearTbl()$ProjPath,".sear"))

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

        PotHocr <- file.path(SearTbl()$ProjPath, ".sear", paste0("filtered_hocr_",str_extract(DataFiles()$bin, "[[:digit:]]{8}_[[:digit:]]{6}"),".rds"))

        PotTimeIndexHocr <- file.path(SearTbl()$ProjPath, ".sear",
                                      paste0("filtered_time_index_hocr_",str_extract(DataFiles()$bin, "[[:digit:]]{8}_[[:digit:]]{6}"),".rds"))

        if (file.exists(PotHocr) & file.exists(PotTimeIndexHocr)) {

          HOCR(read_rds(PotHocr))

          TimeIndexHOCR(read_rds(PotTimeIndexHocr))

        } else {

          validate(need(DataFiles()$bin, label = "Need raw HOCR"))

          Hocr <- read_hocr(DataFiles()$bin)

          # Dont know the logger date format so quick fix with Apla date
          AplaDate <- unique(date(Apla()$DateTime))

          # Posixct object appear to be heavy, same length list of DateTime is heavier (25.8 MB) than the list of HOCR packets (22.2)
          # Computation time arround 2/3 minutes
          TimeIndex <- purrr::map(.x = Hocr, ~ clock::date_time_parse(paste0(AplaDate," ",hms::as_hms(.x$gpstime/1000)), zone = "UTC"))

          HOCR(Hocr)

          TimeIndexHOCR(TimeIndex)

          write_rds(Hocr, PotHocr)

          write_rds(TimeIndex, PotTimeIndexHocr)
        }


      })


# BBFL2 data ----------------------------------------------------------------

      observe({

        # Try to read parsed and filtered applanix data

        #Potential filtered File
        PotBBFL2 <- file.path(SearTbl()$ProjPath,".sear",paste0("filtered_bbfl2_",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

        if (file.exists(PotBBFL2)) {

          BBFL2(read_csv(PotBBFL2))

        } else {

          browser()

          validate(need(MainLog(), label = "Need Raw Applanix data to create mainlog"))

          BBFL2(read_bbfl2(MainLog()))

          # Apla(Apla() %>% filter(
          #   Speed_N <= 4,
          #   BoatSolAzm > 0 & BoatSolAzm < 180
          #   ))

          dir.create(file.path(SearTbl()$ProjPath,".sear"))

          write_csv(BBFL2(), PotBBFL2)
        }

      })

# SeaOWL data -------------------------------------------------------------

      observe({

        # Try to read parsed and filtered applanix data

        #Potential filtered File
        PotSeaOWL <- file.path(SearTbl()$ProjPath,".sear",paste0("filtered_seaowl_",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

        if (file.exists(PotSeaOWL)) {

          SeaOWL(read_csv(PotSeaOWL))

        } else {

          browser()

          validate(need(MainLog(), label = "Need Raw Applanix data to create mainlog"))

          SeaOWL(read_seaowl(MainLog()))

          # Apla(Apla() %>% filter(
          #   Speed_N <= 4,
          #   BoatSolAzm > 0 & BoatSolAzm < 180
          #   ))

          dir.create(file.path(SearTbl()$ProjPath,".sear"))

          write_csv(SeaOWL(), PotSeaOWL)
        }

      })

# SBE19 data ----------------------------------------------------------------

      observe({

        # Try to read parsed and filtered applanix data

        #Potential filtered File
        PotSBE19 <- file.path(SearTbl()$ProjPath,".sear",paste0("filtered_sbe19_",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

        if (file.exists(PotSBE19)) {

          SBE19(read_csv(PotSBE19))

        } else {

          browser()

          validate(need(MainLog(), label = "Need Raw Applanix data to create mainlog"))

          SBE19(read_sbe19(MainLog()))

          # Apla(Apla() %>% filter(
          #   Speed_N <= 4,
          #   BoatSolAzm > 0 & BoatSolAzm < 180
          #   ))

          dir.create(file.path(SearTbl()$ProjPath,".sear"))

          write_csv(SBE19(), PotSBE19)
        }

      })

# Module export -----------------------------------------------------------
    list(
      MainLog = MainLog,
      Apla = Apla,
      HOCR = HOCR,
      TimeIndexHOCR = TimeIndexHOCR
      )

  })
}

## To be copied in the UI
# mod_parse_mtelog_ui("parse_mtelog")

## To be copied in the server
# mod_parse_mtelog_server("parse_mtelog")
