#' obs_ctd UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_sbe19_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("sbe19_l1b")),
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

    output$sbe19_l1b <- renderPlotly({
      validate(need(nrow(Obs$SBE19$L1b) != 0, "No L1b data"))

      PlyFont <- list(family = "times New Roman", size = 18)
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
      #   select(!any_of(c("conductivity", "conservative_temperature", "oxygen_solubility")))%>%
      #   pivot_longer(
      #     cols = any_of(c("temperature", "pressure", "salinity_practical", "salinity_absolute", "oxygen_solubility", "oxygen_concentration", "ph")),
      #     names_to = "parameter",
      #     values_to = "value"
      #   ) %>%
      #   group_by(parameter) %>%
      #   nest()

      ply <- Obs$SBE19$L1b %>%
        mutate(
          Plot = purrr::map2(
            .x = Data,
            .y = parameter,
            ~ plot_ly(
              .x,
              text = ~id,
              customdata = ~ paste0(.y, "_", id)
            ) %>%
              add_markers(
                x = ~ ymd_hms(date_time),
                y = ~value,
                showlegend = F,
                color = ~qc,
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
      p$x$source <- "sbe19_l1b"

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)
    })

    # Get the id of HOCR spectra selected in: selected()$customdata
    observeEvent(
      event_data("plotly_click", source = "sbe19_l1b"),
      label = "qc SBE19",
      ignoreInit = TRUE,
      {
        Selected <- stringr::str_split_fixed(
          event_data("plotly_click", source = "sbe19_l1b")$customdata,
          "_",
          n = 2
        )

        SelParam <- Selected[1]
        SelID <- Selected[2]

        Obs$SBE19$L1b <- Obs$SBE19$L1b %>%
          mutate(
            Data = purrr::map2(
              .x = parameter,
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

    output$DataTable <- DT::renderDataTable(
      {
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
          DT::formatRound(c("oxygen_solubility", "oxygen_concentration", "ph", "pressure", "salinity_absolute", "salinity_practical", "temperature"), digits = 3)
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
