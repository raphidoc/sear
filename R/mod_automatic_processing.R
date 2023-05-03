#' automatic_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_automatic_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("AutoProcess"))
  )
}

#' automatic_processing Server Functions
#'
#' @noRd
mod_automatic_processing_server <- function(id, L1a, CalData, Obs, Settings, MainLog){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$AutoProcess <- renderUI({

      req(L1a$ParsedFiles())

      tagList(
        actionButton(ns("AutoProcess"), "AutoProcess")
      )
    })

    observeEvent(
      input$AutoProcess,
      label = "AutoProcess",
      {
        browser()

        #discretize_time(L1a$Apla()$DateTime)

        MainTime <- MainLog()$DateTime

        i = 1
        j = 1
        n = 1 # number of obs
        while (T) {
          if (length(MainTime) < i + 1) {
            message("This is the end")
            break
          }

          j = i

          x = MainTime[i]
          y = MainTime[j + 1]

          if (interval(x, y) > seconds(10)) {
            i = j + 1
            message("next next")
            next
          }

          # Step 1, iter until interval < 3 sec
          while (interval(x, y) <= seconds(3)) {
            if (length(MainTime) < j + 1) {
              message("This is the end")
              break
            }

            j = j + 1
            y = MainTime[j]
          }

          # At this point. this should always be true
          # Step 3 and 4, processing should happen at this stage
          if (!interval(x, y) > seconds(10) & !interval(x, y) < seconds(3)) {

            TimeInt <- interval(x, y)

            Select <- MainLog()[(MainLog()$DateTime %within% TimeInt),]

            message(paste("L1b process of obs :",n, interval(x, y)))

            Obs$Metadata <- tibble(
              ObsName = "NA",
              ObsType = "NA",
              ObsFlag = "NA",
              DateTime = as.character(mean(Select$DateTime, na.rm = T)),
              DateTimeMin = as.character(min(Select$DateTime, na.rm = T)),
              DateTimeMax = as.character(max(Select$DateTime, na.rm = T)),
              TimeElapsed = as.numeric(interval(DateTimeMin, DateTimeMax)), # in second
              Lon = mean(Select$Lon, na.rm = T),
              Lat = mean(Select$Lat, na.rm = T),
              LonMin = min_geo(Select$Lon, na.rm = T),
              LonMax = max_geo(Select$Lon, na.rm = T),
              LatMin = min_geo(Select$Lat, na.rm = T),
              LatMax = max_geo(Select$Lat, na.rm = T),
              Altitude = mean(as.numeric(Select$Altitude), na.rm = T),
              DistanceRun = pracma::haversine(c(LatMin, LonMin), c(LatMax, LonMax)) * 1000, # in meter
              BoatSolAzm = mean(Select$BoatSolAzm, na.rm = T),
              Comment = "NA",
              UUID = NA
            )


          }

          i = j + 1
        }

      })

  })
}

## To be copied in the UI
# mod_automatic_processing_ui("automatic_processing")

## To be copied in the server
# mod_automatic_processing_server("automatic_processing")
