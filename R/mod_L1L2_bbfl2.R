#' obs_bb3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_bbfl2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("L1b")),
    actionButton(ns("ProcessL2"), "Process L2"),
    DT::DTOutput(ns("DataTable"))
  )
}

#' obs_bb3 Server Functions
#'
#' @noRd
mod_L1L2_bbfl2_server <- function(id, Obs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$L1b <- renderPlotly({

      # BBFL2L1bNest <- Obs$BBFL2$L1b %>%
      #   select(!any_of(c("SN")))%>%
      #   pivot_longer(
      #     cols = any_of(c("Bb_700", "Chl", "FDOM")),
      #     names_to = "Parameter",
      #     values_to = "Value"
      #   ) %>%
      #   group_by(Parameter) %>%
      #   nest()


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

      ply <- Obs$BBFL2$L1b %>%
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
                x = ~DateTime,
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

      p <- subplot(ply$Plot, nrows = nrow(ply), shareX = TRUE, titleY = TRUE)%>%
        event_register("plotly_click")

      # Set source for selection event
      p$x$source <- "BBFL2L1b"

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)

    })

    observeEvent(
      event_data("plotly_click", source = "BBFL2L1b"),
      label = "QC BBFL2",
      ignoreInit = TRUE,
      {

        Selected <- stringr::str_split_fixed(
          event_data("plotly_click", source = "BBFL2L1b")$customdata,
          "_(?=[:digit:]{1,}?$)",
          n = 2
        )

        SelParam <- Selected[1]
        SelID <- Selected[2]

        Obs$BBFL2$L1b <- Obs$BBFL2$L1b %>%
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
        Obs$BBFL2$L2 <- L2_param_val(Obs$BBFL2$L1b)
      }
    )

    output$DataTable <- DT::renderDataTable({

      validate(need(nrow(Obs$BBFL2$L2) != 0, "Process L2 to dispaly observation statistics"))

      Obs$BBFL2$L2 %>% DT::datatable(extensions = c("Buttons", "Scroller", "Select"),
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
        DT::formatRound(c("NTU", "PC", "PE"), digits=3)

    },
    server = FALSE,
    editable = F
    )

  })
}

## To be copied in the UI
# mod_L1L2_bbfl2_ui("L1L2_bbfl2")

## To be copied in the server
# mod_L1L2_bbfl2_server("L1L2_bbfl2")
