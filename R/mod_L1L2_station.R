#' L1L2_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1L2_station_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(outputId = ns("HOCRL1b")),
    plotlyOutput(ns("AOPs"), height = 250)

  )
}

#' L1L2_station Server Functions
#'
#' @noRd
mod_L1L2_station_server <- function(id, L1bDataLong){

  stopifnot(is.reactive(L1bDataLong))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$HOCRL1b <- renderUI({

      req(L1bDataLong())

        tagList(
          plotlyOutput(ns("HOCRL1bplot"), height = 250),
          actionButton(ns("ProcessL2"), "ProcessL2")#,
          #DT::dataTableOutput(ns("DataTable"))
        )


    })

    # Tibble for storing QC value initiated with 1 = good
    QCData <- reactiveVal({

      L1bDataLong()$AproxData[[1]] %>%
        select(DateTime, ID) %>%
        unique() %>%
        mutate(QC = "1")

      #[Data()$AproxData[[1]]$ID %in% Selected(),]

    })

    # Get the ID of HOCR spectra selected in: selected()$customdata
    Selected <- reactiveVal({})

    observeEvent(event_data('plotly_click', source = "HOCRL1b"), {
      Selected(event_data('plotly_click', source = "HOCRL1b")$customdata)

      tmp <- QCData()

      # Change value for selected spectrum ID
      if (tmp$QC[tmp$ID %in% Selected()] == "1") {
        tmp$QC[tmp$ID %in% Selected()] <- 0
      } else if (tmp$QC[tmp$ID %in% Selected()] == "0") {
        tmp$QC[tmp$ID %in% Selected()] <- 1
      }

      QCData(tmp)
    })

    L1bData <- reactive({
      L1bDataLong() %>%
        select(Instrument, SN, AproxData) %>%
        mutate(AproxData = purrr::map(AproxData, ~left_join(., QCData(), by = c("DateTime", "ID"))))
    })

    output$HOCRL1bplot <- renderPlotly({

      req(L1bDataLong())
      req(QCData())


      PlyFont <- list(family="Times New Roman", size = 18)
      BlackSquare <- list(type = "rect", fillcolor = "transparent",line = list(width = 0.5), xref = "paper", yref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 1 )


      ply <- L1bData() %>%
        #filter(str_detect(Instrument, "HPL")) %>%
        mutate(Plot = purrr::map2(.x = AproxData, .y = SN, ~
                                    plot_ly(.x, text = ~ID, customdata = ~ID) %>%
                                    add_lines(x = ~Wavelength, y = ~Channels , name = ~QC, showlegend = F, color = ~QC, colors = c("1" = "seagreen", "0" = "red")) %>%
                                    add_annotations(
                                      text = ~.y,
                                      x = 0.5,
                                      y = 1,
                                      yref = "paper",
                                      xref = "paper",
                                      xanchor = "middle",
                                      yanchor = "top",
                                      showarrow = FALSE,
                                      font = list(size = 15)
                                    ) %>%
                                    layout(
                                      shapes = BlackSquare,
                                      yaxis = list(rangemode = "nonnegative"
                                                   #title = list(text = ~paste0(unique(.x$Type), unique(.x$Units)))
                                      ),
                                      xaxis = list(rangemode = "nonnegative")
                                    )# %>%
                                    #event_register('plotly_selected')
        ))

      Lu <- ply %>%
        filter(str_detect(Instrument, "HPL")) %>%
        subplot(shareX = T, shareY = T)

      Es <- ply %>%
        filter(str_detect(Instrument, "HSE"))

      Es <- Es$Plot

      p <- subplot(Es[[1]], Lu, nrows = 2, margin = 0.035) %>%
        add_annotations(
          text = ~TeX("\\text{Wavelength [nm]}"),
          x = 0.5,
          y = -0.1,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 18)
        ) %>%
        layout(
          font = PlyFont,
          yaxis = list(title = list(text = TeX("\\text{E}_\\text{s}"))),
          yaxis2 = list(title = list(text = TeX("\\text{L}_\\text{u}")))#,
          #xaxis3 = list(title = list(text = TeX("\\text{Wavelength}")))
        ) %>%
        config(mathjax = "local", displayModeBar = T) %>%
        event_register('plotly_click')

      # Set source for selection event
      p$x$source <- "HOCRL1b"

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)
    })

    L2Data <- eventReactive(input$ProcessL2, {



      L1bDataWide <- L1bData() %>%
        mutate(AproxData = purrr::map(
          AproxData,
          ~ pivot_wider(
            .,
            names_from = all_of(c("Type", "Wavelength")),
            names_sep = "_",
            values_from = Channels
          ) %>%
          ungroup())) %>%
        ungroup()

      L1bDataWide <- L1bDataWide %>%
        mutate(AproxData = purrr::map(AproxData, ~filter(., QC == "1")))

      L1bDataWide <- L1bDataWide %>%
        mutate(AproxData = purrr::map(AproxData, ~summarise(.x, across(.cols = !matches("ID|QC"), ~ mean(.x, na.rm =T)))))

      ### Approx wavelength
      L1bAverageLong <- L1bDataWide %>%
        mutate(AproxData = purrr::map(
          AproxData,
          ~pivot_longer(
            .,
            cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
            values_to = "Channels",
            names_to = c("Type","Wavelength"),
            names_sep = "_",
            #names_prefix = "[[:alpha:]]{2}_",
            names_transform = list(Wavelength = as.numeric)
          )
        ))

      WaveSeq <- seq(353,800,3)

      approx_wave <- function(., WaveSeq) {

        tbl <- tibble(
          DateTime = unique(.$DateTime),
          Type = unique(.$Type),
          Wavelength = WaveSeq
        )

        for (i in seq_along(colnames(.))[-1:-3]) {

          coord <- approx(x = .[[3]], y = .[[i]], xout = WaveSeq, method = "linear")

          tbl <- bind_cols(tbl, x = coord[[2]])
          colnames(tbl)[i] <- colnames(.)[i]
        }

        tbl

        #tbl %>% mutate(ID = seq_along(TimeSeq))
      }

      L1bAproxLong <- L1bAverageLong %>%
        mutate(IntData = purrr::map(AproxData, ~ approx_wave(., WaveSeq)))

      L1bAproxWide <- L1bAproxLong %>%
        mutate(IntData = purrr::map(
          IntData,
          ~ pivot_wider(
            .,
            names_from = all_of(c("Type", "Wavelength")),
            names_sep = "_",
            values_from = Channels
          ))) %>%
        ungroup()

      Es <- L1bAproxWide %>%
        select(!AproxData) %>%
        filter(SN == "1397") %>%
        unnest(cols = c(IntData)) %>%
        select(!matches("Instrument|SN|DateTime"))

      LuZ1 <- L1bAproxWide %>%
        select(!AproxData) %>%
        filter(SN == "1416") %>%
        unnest(cols = c(IntData))%>%
        select(!matches("Instrument|SN|DateTime"))

      LuZ2 <- L1bAproxWide %>%
        select(!AproxData) %>%
        filter(SN == "1415") %>%
        unnest(cols = c(IntData))%>%
        select(!matches("Instrument|SN|DateTime"))

      DeltaDepth <- 0.30 # 30 cm

      KLuWide <- (log(LuZ1)-log(LuZ2))/DeltaDepth

      KLuWide <- rename_with(KLuWide, ~ str_replace(.x, "LU", "KLu"))

      KLuLong <- KLuWide %>%
        pivot_longer(
          .,
          cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
          values_to = "KLu",
          names_to = c("Wavelength"),
          #names_sep = "_",
          names_prefix = "[[:alpha:]]{3}_",
          names_transform = list(Wavelength = as.numeric)
        )

      LuZ1Depth <- 0.10 # 10 cm

      Lw <- 0.54 * LuZ1 / exp(-LuZ1Depth*KLuWide)
      RrsWide <- Lw / Es

      RrsLong <- RrsWide %>%
        pivot_longer(
          .,
          cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
          values_to = "Rrs",
          names_to = c("Wavelength"),
          #names_sep = "_",
          names_prefix = "[[:alpha:]]{2}_",
          names_transform = list(Wavelength = as.numeric)
        )

      L2Data <- left_join(RrsLong, KLuLong, by = "Wavelength")

      L2Data

    })

    output$AOPs <- renderPlotly({

      req(L2Data())

      Rrsplot <- L2Data() %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~Rrs, showlegend = F)

      KLuplot <- L2Data() %>%
        plot_ly() %>%
        add_lines(x = ~Wavelength, y = ~KLu, showlegend = F)

      subplot(Rrsplot, KLuplot)

      })



    #output$HOCRL2plot <- render


    # DataTable
  #   output$DataTable <- DT::renderDataTable(
  #     DT::datatable(SelectedData(),
  #                   extensions = c("Buttons", "Scroller", "Select"),
  #                   filter = "top",
  #                   escape = TRUE, rownames = FALSE,
  #                   style = "bootstrap",
  #                   class = "compact",
  #                   options = list(
  #                     dom = "Brtip",
  #                     select = list(style = 'os', items = 'row'),
  #                     buttons = list(I("colvis"),"selectNone","csv"),
  #                     columnDefs = list(
  #                       list(
  #                         visible = FALSE,
  #                         targets = c(0,2,3)
  #                       )),
  #                     deferRender = TRUE,
  #                     scrollY = 100,
  #                     pageLength = 10,
  #                     scroller = TRUE
  #                   ),
  #                   selection = "none",
  #                   editable = F
  #     ),
  #     server=FALSE,
  #     editable=T
  #   )
  #
   })
}

## To be copied in the UI
# mod_L1L2_station_ui("L1L2_station")

## To be copied in the server
# mod_L1L2_station_server("L1L2_station")
