#' obs_ctd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_sbe19_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("SBE19L1b")),
    actionButton(ns("ProcessL2"), "Process L2"),
    DT::DTOutput(ns("DataTable"))
  )
}

#' obs_ctd Server Functions
#'
#' @noRd
mod_L1L2_sbe19_server <- function(id, Obs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$SBE19L1b <- renderPlotly({
      validate(need(nrow(Obs$SBE19$L1b) != 0, "No L1b data"))

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

      # SBE19nest <- Obs$SBE19$L1b %>%
      #   select(!any_of(c("Conductivity", "CT", "O2Sol")))%>%
      #   pivot_longer(
      #     cols = any_of(c("Temperature", "Pressure", "SP", "SA", "OxSol", "Oxygen", "pH")),
      #     names_to = "Parameter",
      #     values_to = "Value"
      #   ) %>%
      #   group_by(Parameter) %>%
      #   nest()

      ply <- Obs$SBE19$L1b %>%
        mutate(
          Plot = purrr::map2(
            .x = Data,
            .y = Parameter,
            ~ plot_ly(
                .x,
                text = ~ID,
                customdata = ~paste0(.y,"_",ID)
              ) %>%
              add_markers(
                x = ~ymd_hms(DateTime),
                y = ~Value,
                showlegend = F,
                color = ~QC,
                colors = c("1" = "seagreen", "0" = "red")
              ) %>%
              layout(
                shapes = BlackSquare,
                yaxis = list(title = list(text = .y))
              )
          )
        )

      p <- subplot(ply$Plot, nrows = nrow(ply), shareX = TRUE, titleY = TRUE) %>%
        event_register("plotly_click")

      # Set source for selection event
      p$x$source <- "SBE19L1b"

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)

    })

    # Get the ID of HOCR spectra selected in: selected()$customdata
    observeEvent(
      event_data("plotly_click", source = "SBE19L1b"),
      label = "QC SBE19",
      ignoreInit = TRUE,
      {

        Selected <- stringr::str_split_fixed(
          event_data("plotly_click", source = "SBE19L1b")$customdata,
          "_",
          n = 2
          )

        SelParam <- Selected[1]
        SelID <- Selected[2]

       Obs$SBE19$L1b <- Obs$SBE19$L1b %>%
         mutate(
           Data = purrr::map2(
             .x = Parameter,
             .y = Data,
             ~ if (.x == SelParam) {
               qc_shift(.y, SelID)
             } else {
               .y
             }
           )
         )

      }
    )

    observeEvent(
      input$ProcessL2,
      {
        Obs$SBE19$L2 <- L2_param_val(Obs$SBE19$L1b)
      }
    )

    output$DataTable <- DT::renderDataTable({

      validate(need(nrow(Obs$SBE19$L2) != 0, "Process L2 to dispaly observation statistics"))

      DT::datatable(Obs$SBE19$L2,
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
        DT::formatRound(c("OxSol", "Oxygen", "pH", "Pressure", "SA", "SP", "Temperature"), digits=3)

      },
      server = FALSE,
      editable = F
    )



    # output$StatTable <- renderUI({
    #   validate(need(nrow(Obs$SBE19$L2) != 0, "Process L2 to dispaly observation statistics"))
    #
    # })



  })
}

## To be copied in the UI
# mod_L1L2_sbe19_ui("L1L2_sbe19")

## To be copied in the server
# mod_L1L2_sbe19_server("L1L2_sbe19")
