#' filter_cut UI Function
#'
#' @description If data points are selected remove them from filtered_* files.
#' If no data points are selected keep only the displayed (SubApla) points.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_cut_ui <- function(id){
  ns <- NS(id)
  tagList(

    actionButton(ns("Cut"), "Cut", icon = icon("glyphicon glyphicon-scissors", lib = "glyphicon"))

  )
}

#' filter_cut Server Functions
#'
#' @noRd
mod_filter_cut_server <- function(id, SearTbl, DataFiles, SelData, Apla){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(
      input$Cut,
      {

        browser()

        PotApla <- file.path(
          SearTbl()$ProjPath,".sear",
          paste0("filtered_apla_",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

        PotHocr <- file.path(
          SearTbl()$ProjPath, ".sear",
          paste0("filtered_hocr_",str_extract(DataFiles()$bin, "[[:digit:]]{8}_[[:digit:]]{6}"),".rds"))

        PotTimeIndexHocr <- file.path(
          SearTbl()$ProjPath, ".sear",
          paste0("filtered_time_index_hocr_",str_extract(DataFiles()$bin, "[[:digit:]]{8}_[[:digit:]]{6}"),".rds"))

        if (any(!file.exists(PotApla, PotHocr, PotTimeIndexHocr))) {

          FileNotFound <- c(PotApla, PotHocr, PotTimeIndexHocr)[which(file.exists(c(PotApla, PotHocr, PotTimeIndexHocr)))]

          # Feedback to the user
          session$sendCustomMessage(
            type = 'testmessage',
            message = purrr::map_chr(FileNotFound, ~ glue::glue("File :", .x, " not found"))
          )

          invalidateLater(1)

        } else {

          Apla(Apla()[Apla()$DateTime %in% SelData$SubApla()$DateTime, ])

          write_csv(Apla(), PotApla, append = F)

          #Hocr <- Hocr[TimeIndex %in% Apla()$DateTime]

          #TimeIndex <- TimeIndex[TimeIndex %in% Apla()$DateTime]

        }



      }
    )

  })
}

## To be copied in the UI
# mod_filter_cut_ui("filter_cut")

## To be copied in the server
# mod_filter_cut_server("filter_cut")
