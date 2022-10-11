#' filter_trim UI Function
#'
#' @description If data points are selected remove them from filtered_* files.
#' If no data points are selected keep only the displayed (SubApla) points.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_trim_ui <- function(id){
  ns <- NS(id)
  tagList(

    actionButton(ns("trim"), "trim", icon = icon("glyphicon glyphicon-scissors", lib = "glyphicon"))

  )
}

#' filter_trim Server Functions
#'
#' @noRd
mod_filter_trim_server <- function(id, SearTbl, DataFiles, SelData, Apla){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(
      input$trim,
      {
        PotApla <- file.path(
          SearTbl()$ProjPath,".sear",
          paste0("filtered_apla_",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

        PotAplaTrim <- file.path(
          SearTbl()$ProjPath,".sear",
          paste0("filtered_apla_trim",str_extract(DataFiles()$txt, "[[:digit:]]{8}_[[:digit:]]{6}"),".csv"))

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

        } else if (any(class(tryCatch(SelData$SelID(), error = function(e) e)) == "error")) {
          # if no data is selected SelData$SelID() throw an error

          modal_confirm <- modalDialog(
            "No data is selected, only the data visible on the map will be kept, are you sure you want to continue?",
            title = "Deleting raw data",
            footer = tagList(
              actionButton(ns("cancel"), "Cancel"),
              actionButton(ns("ok"), "Delete", class = "btn btn-danger")
            )
          )

          observeEvent(req(input$Delete), {
            showModal(modal_confirm)
          })

          # If user confirm delete
          observeEvent(input$ok, {
            removeModal()

            Apla(Apla()[Apla()$DateTime %in% SelData$SubApla()$DateTime, ])

            write_csv(Apla(), PotAplaTrim, append = F)

            # Feedback to the user
            # session$sendCustomMessage(
            #   type = 'testmessage',
            #   message =
            #     glue::glue(
            #       LineDel," line deleted\n")
            # )
          })

          # If user cancel
          observeEvent(input$cancel, {
            removeModal()
          })

        } else if (class(SelData$SelID()) == "numeric") {

          modal_confirm <- modalDialog(
            "The selected data will be deleted, are you sure you want to continue?",
            title = "Deleting raw data",
            footer = tagList(
              actionButton(ns("cancel"), "Cancel"),
              actionButton(ns("ok"), "Delete", class = "btn btn-danger")
            )
          )

          observeEvent(req(input$Delete), {
            showModal(modal_confirm)
          })

          # If user confirm delete
          observeEvent(input$ok, {
            removeModal()

            Apla(Apla()[which(!Apla()$ID %in% SelData$SelID()), ])

            write_csv(Apla(), PotAplaTrim, append = F)

            # Feedback to the user
            # session$sendCustomMessage(
            #   type = 'testmessage',
            #   message =
            #     glue::glue(
            #       LineDel," line deleted\n")
            # )
          })

          # If user cancel
          observeEvent(input$cancel, {
            removeModal()
          })
        }
      }
    )

  })
}

## To be copied in the UI
# mod_filter_trim_ui("filter_trim")

## To be copied in the server
# mod_filter_trim_server("filter_trim")
