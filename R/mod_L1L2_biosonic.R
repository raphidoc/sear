#' L1L2_biosonic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_biosonic_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("L1b"), height = 320),
    actionButton(ns("ProcessL2"), "Process L2"),
    DT::DTOutput(ns("DataTable"))
  )
}

#' L1L2_biosonic Server Functions
#'
#' @noRd
mod_L1L2_biosonic_server <- function(id, Obs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$L1b <- renderPlotly({
      validate(need(nrow(Obs$BioSonic$L1b) != 0, "No L1b data"))

      PlyFont <- list(family = "Times New Roman", size = 18)
      BlackSquare <- list(
        type = "rect",
        fillcolor = "transparent",
        line = list(width = 0.5),
        xref = "paper",
        yref = "paper",
        x0 = 0,
        x1 = 1,
        y0 = 0,
        y1 = 1
      )

      pal <- c("Watercraft"="blue", "Canopy"="green", "Bottom"="brown")

      ply <- Obs$BioSonic$L1b %>%
        mutate(
          Distance = seq(from = 0, to = Obs$MetadataL2$DistanceRun, along.with = DateTime)
        ) %>%
        plot_ly(
          text = ~DateTime,
          x = ~Distance,
          colors = pal
        ) %>%
        add_lines(
          y = ~Altitude_mReMsl,
          color = "Watercraft"
        ) %>%
        add_lines(
          y = ~BottomElevation_m+PlantHeight_m,
          color = "Canopy"
        ) %>%
        add_lines(
          y = ~BottomElevation_m,
          color = "Bottom"
        ) %>%
        layout(
          shapes = BlackSquare,
          xaxis = list(title = list(text = "Distance [m]"))
          #yaxis = list(range = list(~min(BottomElevation_m, na.rm = TRUE), 0))
        )

      # Save graph
      #save_image(ply, file=file.path(path.expand("~"), "sear_figure", "SBES.svg"), scale = 3 , height = 720, width = 1280)

      # Iframe to render svg properly
      widgetframe::frameableWidget(ply)

    })

    observeEvent(
      input$ProcessL2,
      {

        test <- Obs$BioSonic$L1b %>% summarise(
          Lon = mean(Lon),
          Lat = mean(Lat),
          DateTime = mean(DateTime),
          Altitude_mReMsl = mean(Altitude_mReMsl),
          BottomElevation_m = mean(BottomElevation_m),
          PlantHeight_m = mean(PlantHeight_m),
          PercentCoverage = mean(PercentCoverage)
        )

        Obs$BioSonic$L2 <- test
      }
    )

    output$DataTable <- DT::renderDataTable({

      validate(need(nrow(Obs$BioSonic$L2) != 0, "Process L2 to dispaly observation statistics"))

      DT::datatable(Obs$BioSonic$L2,
                    extensions = c("Buttons", "Scroller", "Select"),
                    # filter = "top",
                    escape = TRUE, rownames = FALSE,
                    style = "bootstrap",
                    class = "compact",
                    options = list(
                      dom = "Brtip",
                      select = list(style = "os", items = "row"),
                      buttons = list(I("colvis"), "selectNone", "csv"),
                      columnDefs = list(
                        list(
                          visible = FALSE,
                          targets = c()
                        )
                      ),
                      deferRender = TRUE,
                      scrollY = 100,
                      pageLength = 10,
                      scroller = TRUE
                    ),
                    selection = "none",
                    editable = F
      ) %>%
        DT::formatRound(c("Lat", "Lon"), digits=6) %>%
        DT::formatRound(c("Altitude_mReMsl", "BottomElevation_m", "PlantHeight_m"), digits=3)

    },
    server = FALSE,
    editable = F
    )

  })
}

## To be copied in the UI
# mod_L1L2_biosonic_ui("L1L2_biosonic")

## To be copied in the server
# mod_L1L2_biosonic_server("L1L2_biosonic")
