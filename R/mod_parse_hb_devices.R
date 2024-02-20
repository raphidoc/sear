#' hb_devices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parse_hb_devices_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("Load"))
  )
}

#' hb_devices Server Functions
#'
#' @noRd
mod_parse_hb_devices_server <- function(id, SearProj, ParsedFiles) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Load <- renderUI({
      req(SearProj())

      fileInput(ns("Files"), "Choose HydroBall Devices .txt Files", accept = c(".txt", ".TXT"), multiple = T)
    })

    observeEvent(
      input$Files,
      {
        RawDir <- file.path(SearProj()$ProjPath, "sear", "data", "raw")

        dir.create(RawDir, recursive = TRUE)

        Files <- input$Files %>%
          mutate(
            rawpath = file.path(RawDir, name)
          )

        file.copy(Files$datapath, Files$rawpath)

        TimeTag <- str_extract(Files$name, "[:digit:]{2}_[:digit:]{2}_[:digit:]{2}-[:digit:]{4}_[:digit:]{2}_[:digit:]{2}")

        Time <- str_split(TimeTag, "-")[[1]][1]
        Date <- str_split(TimeTag, "-")[[1]][2]

        DateTime <- ymd_hms(paste(Date, Time))

        ParsedDir <- file.path(SearProj()$ProjPath, "sear", "data", "parsed")

        dir.create(ParsedDir, recursive = TRUE)

        # No date is available in the devices files, take it from the file name
        HBDevices <- read_hb_devices(Files$rawpath, date(DateTime))

        PotHBDevices <- file.path(ParsedDir, paste0("hb_devices_", format(DateTime, "%Y%m%d_%H%M%S"), ".csv"))

        write_csv(HBDevices, PotHBDevices)
      }
    )

    return(reactive(input$Files))
  })
}

## To be copied in the UI
# mod_parse_hb_devices_ui("parse_hb_devices")

## To be copied in the server
# mod_parse_hb_devices_server("parse_hb_devices")
