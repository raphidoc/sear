#' obs_hocr UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1hocr_l2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("hocr_l1b"), height = 350),
    uiOutput(ns("ProL2")),
    checkboxInput(ns("Loess"), "Loess", value = TRUE, width = NULL),
    numericInput(ns("Span"), "span", 0.1, step = 0.01),
    plotlyOutput(ns("AOPs"), height = 320)
  )
}

#' obs_hocr Server Functions
#'
#' @noRd
mod_L1hocr_l2_server <- function(id, Obs, Settings) {
  # stopifnot(is.reactive(L1bData))

  PlyFont <- list(family = "times New Roman", size = 14)
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

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # HOCR Es and Lu plot
    output$hocr_l1b <- renderPlotly({
      validate(need(nrow(Obs$HOCR$L1b) != 0, "No L1b data"))

      Obs$metadata_l1b %>% mutate(date_time = as.numeric(ymd_hms(date_time)))

      # Add pitch and roll metadata
      # TODO make this code clearer ... possibly with purrr::map ?
      # Enhanced <- Obs$HOCR$L1b %>%
      #   unnest(cols = c(cal_data)) %>%
      #   mutate(date_time = as.numeric(date_time)) %>%
      #   fuzzyjoin::difference_left_join(
      #     Obs$metadata_l1b %>% mutate(date_time = as.numeric(ymd_hms(date_time))),
      #     by = c("date_time"),
      #     max_dist = 1,
      #     distance_col = "Distance"
      #   ) %>%
      #   group_by(id, Distance) %>%
      #   nest() %>%
      #   group_by(id) %>%
      #   slice(which.min(Distance)) %>%
      #   unnest(cols = c(data)) %>%
      #   group_by(instrument, sn) %>%
      #   nest(.key = "cal_data")

      ply <- Obs$HOCR$L1b %>%
        arrange(sn) %>%
        # filter(str_detect(instrument, "HPL")) %>%
        mutate(
          Plot = purrr::map2(
            .x = cal_data,
            .y = sn,
            ~ plot_ly(
              .x %>% group_by(id),
              text = ~ paste0(
                "<b>id</b>: ", id, "<br>"
              ),
              customdata = ~id
            ) %>%
              add_lines(
                x = ~wavelength,
                y = ~channel,
                #name = ~qc,
                showlegend = F,
                color = ~qc,
                colors = c("1" = "seagreen", "0" = "red")
              ) %>%
              add_annotations(
                text = ~paste0("N = ", length(unique(id))),
                x = 0.8,
                y = 1,
                yref = "paper",
                xref = "paper",
                xanchor = "middle",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 15)
              ) %>%
              add_annotations(
                text = ~.y,
                x = 0.08,
                y = 1,
                yref = "paper",
                xref = "paper",
                xanchor = "middle",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 15)
              ) %>%
              layout(
                shapes = BlackSquare
              )
          )
        )

      Lu <- ply %>%
        filter(str_detect(instrument, "HPL|HSL")) %>% # HSL is not supposed to be Lu ...
        subplot(shareX = T, shareY = T)

      Es <- ply %>%
        filter(str_detect(instrument, "HSE"))

      Es <- Es$Plot

      p <- subplot(Es[[1]], Lu, nrows = 2, margin = 0.038, shareX = T, shareY = T) %>%
        add_annotations(
          text = ~"wavelength [nm]",
          x = 0.5,
          y = -0.14,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 18)
        ) %>%
        layout(
          font = PlyFont,
          yaxis = list(title = list(
            text = "Es [uW.cm-2.nm-1]" # TeX("\\text{e}_\\text{s}")
          )),
          yaxis2 = list(title = list(
            text = "Lu [uW.cm-2.nm-1.sr-1]" # TeX("\\text{L}_\\text{u}")
          )) # ,
          # xaxis3 = list(title = list(text = TeX("\\text{wavelength}")))
        ) %>%
        config(mathjax = "cdn", displayModeBar = T) %>%
        event_register("plotly_click")

      # Set source for selection event
      p$x$source <- "hocr_l1b"

      # Save graph
      # save_image(p, file=file.path(path.expand("~"), "sear_figure", "L1b.svg"), scale = 3, height = 720, width = 1280)

      # Iframe to render svg properly
      widgetframe::frameableWidget(p)
    })

    # Get the id of HOCR spectra selected in: selected()$customdata
    observeEvent(
      event_data("plotly_click", source = "hocr_l1b"),
      label = "qc HOCR",
      ignoreInit = TRUE,
      {
        Selected <- event_data("plotly_click", source = "hocr_l1b")$customdata

        Obs$HOCR$L1b <- Obs$HOCR$L1b %>% mutate(cal_data = purrr::map(cal_data, ~ qc_shift(., Selected)))
      }
    )

    output$ProL2 <- renderUI({
      validate(need(
        {
          Settings$HOCR$WaveMin() &
          Settings$HOCR$WaveMax() &
          Settings$HOCR$WaveStep() &
          Settings$HOCR$Z1Depth() &
          Settings$HOCR$Z1Z2Depth()
        },
        message = "Need HOCR settings"
      ))

      actionButton(ns("ProcessL2"), "Process L2")
    })


    # Compute AOPs ------------------------------------------------------------
    observeEvent(
      input$ProcessL2,
      {
        wave_seq <- seq(
          Settings$HOCR$WaveMin(),
          Settings$HOCR$WaveMax(),
          Settings$HOCR$WaveStep()
        )

        if (any(Obs$SBE19$L1b$parameter == "pressure")) {
          message("Taking z1 from CTD")
          pressure <- Obs$SBE19$L1b$Data[Obs$SBE19$L1b$parameter == "pressure"][[1]]$value

          z1 <- tibble(
            z = gsw::gsw_z_from_p(p = pressure, latitude = Obs$metadata_l2$lat),
            z1 = z + 0.1
          ) %>%
            select(z1) %>%
            summarise(
              across(where(is.numeric), list(median = median, sd = ~ sd(.x, na.rm=T)), .names= "{.col}_{.fn}")
            )


        } else {
          message("Taking single z1 from setting")
          z1 <- tibble(
            z1_median = Settings$HOCR$Z1Depth(),
            z1_sd = 0
          )
        }

        z2z1 <- Settings$HOCR$Z1Z2Depth()

        Obs$HOCR$L2 <- hocr_l2(
          Obs$HOCR$L1b, wave_seq, z1, z2z1,
          input$Loess, input$Span, Obs
        )
      }
    )

    output$AOPs <- renderPlotly({
      # req(l2_data())

      validate(need(nrow(Obs$HOCR$L2) != 0, "Process L2 to display AOPs"))

      rrs_plot <- Obs$HOCR$L2 %>%
        plot_ly(
          x = ~wavelength,
          y = ~rrs_median
          ) %>%
        add_lines(
          showlegend = T
          ) %>%
        add_ribbons(
          ymin = ~
           rrs_median -
            rrs_mad,
          ymax = ~
           rrs_median +
            rrs_mad,
          name = "unc_rrs",
          line = list(color = 'rgba(105, 159, 245, 0.2)'),
          fillcolor = 'rgba(105, 159, 245, 0.1)',
          showlegend = F
        ) %>%
        layout(shapes = BlackSquare)

      if (any(str_detect(names(Obs$HOCR$L2), "qwip_score"))) {
        Rrsplot <- Rrsplot %>%
          add_annotations(
            text = ~ paste("QWIP:", unique(Obs$metadata_l2$qwip_score)),
            x = 0.01,
            y = 1,
            yref = "paper",
            xref = "paper",
            xanchor = "middle",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 15)
          )
      }


      if (any(str_detect(names(Obs$HOCR$L2), "Rrs_loess"))) {
        Rrsplot <- Rrsplot %>%
          add_trace(
            x = ~wavelength,
            y = ~Rrs_loess,
            type = "scatter",
            mode = "lines",
            line = list(dash = "dash", color = "red")
          )
      }

      rrs_unc_plot <- Obs$HOCR$L2 %>%
        plot_ly(x = ~ wavelength) %>%
        add_trace(
          type = 'scatter', mode = 'lines', fill = 'tonexty',
          y = ~ rrs_mad * rrs_luz1_rel_unc ,
          name = TeX("\\sigma L_\\text{u}(z1)"),
          line = list(color = 'rgba(105, 159, 245, 0.3)'),
          fillcolor = 'rgba(105, 159, 245, 0.3)',
          legendgroup = 1,
          showlegend = F
        ) %>%
        add_trace(
          type = 'scatter', mode = 'lines', fill = 'tonexty',
          y = ~
            rrs_mad * rrs_luz1_rel_unc +
            rrs_mad * rrs_luz2_rel_unc ,
          name = TeX("\\sigma L_\\text{u}(z2)"),
          line = list(color = 'rgba(6, 58, 143, 0.3)'),
          fillcolor = 'rgba(6, 33, 143, 0.3)',
          legendgroup = 2,
          showlegend = F
        ) %>%
        add_trace(
          type = 'scatter', mode = 'lines', fill = 'tonexty',
          y = ~
            rrs_mad * rrs_luz1_rel_unc +
            rrs_mad * rrs_luz2_rel_unc +
            rrs_mad * rrs_z1_rel_unc,
          name = TeX("\\sigma z1"),
          line = list(color = 'rgba(74, 176, 100, 0.3)'),
          fillcolor = 'rgba(74, 176, 100, 0.3)',
          legendgroup = 3,
          showlegend = F
        ) %>%
        add_trace(
          type = 'scatter', mode = 'lines', fill = 'tonexty',
          y = ~
            rrs_mad * rrs_luz1_rel_unc +
            rrs_mad * rrs_luz2_rel_unc +
            rrs_mad * rrs_z1_rel_unc +
            rrs_mad * rrs_temp_rel_unc,
          name = TeX("\\sigma \\text{temperature}"),
          line = list(color = 'rgba(255, 0, 0, 0.3)'),
          fillcolor = 'rgba(255, 0, 0, 0.3)',
          legendgroup = 4,
          showlegend = F
        ) %>%
        add_trace(
          type = 'scatter', mode = 'lines', fill = 'tonexty',
          y = ~
            rrs_mad * rrs_luz1_rel_unc +
            rrs_mad * rrs_luz2_rel_unc +
            rrs_mad * rrs_z1_rel_unc +
            rrs_mad * rrs_temp_rel_unc +
            rrs_mad * rrs_sal_rel_unc,
          name = TeX("\\sigma \\text{salinity}"),
          line = list(color = 'rgba(0, 0, 255, 0.3)'),
          fillcolor = 'rgba(0, 0, 255, 0.3)',
          legendgroup = 5,
          showlegend = F
        ) %>%
        add_trace(
          type = 'scatter', mode = 'lines', fill = 'tonexty',
          y = ~
            rrs_mad * rrs_luz1_rel_unc +
            rrs_mad * rrs_luz2_rel_unc +
            rrs_mad * rrs_z1_rel_unc +
            rrs_mad * rrs_temp_rel_unc +
            rrs_mad * rrs_sal_rel_unc +
            rrs_mad * rrs_es_rel_unc,
          name = TeX("\\sigma E_\\text{d}(0^+)"),
          line = list(color = 'rgba(169, 74, 176, 0.3)'),
          fillcolor = 'rgba(169, 74, 176, 0.3)',
          legendgroup = 6,
          showlegend = F
        ) %>%
        layout(
          xaxis = list(title=TeX("\\text{Wavelength}")),
          yaxis = list(title=TeX("R_\\text{rs} [\\text{sr}^{-1}]")),
          shapes=list(BlackSquare)
        ) %>%
        config(mathjax = "cdn", displayModeBar = F)

      rrs_unc_plot

      # KLuplot <- Obs$HOCR$L2 %>%
      #   plot_ly(x = ~wavelength, y = ~klu_mean) %>%
      #   add_lines(showlegend = T) %>%
      #   layout(shapes = BlackSquare)
      #
      # if (any(str_detect(names(Obs$HOCR$L2), "KLu_loess"))) {
      #   KLuplot <- KLuplot %>%
      #     add_trace(x = ~wavelength, y = ~KLu_loess, type = "scatter", mode = "lines", line = list(dash = "dash", color = "red"))
      # }

      ply <- subplot(
        rrs_plot,
        rrs_unc_plot,
        shareX = T,
        titleX = F,
        margin = 0.05
        ) %>%
        add_annotations(
          text = ~"wavelength [nm]", # TeX("\\text{wavelength [nm]}"),
          x = 0.5,
          y = -0.15,
          yref = "paper",
          xref = "paper",
          xanchor = "middle",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 18)
        ) %>%
        layout(
          font = PlyFont,
          yaxis = list(title = list(
            text = "Rrs [sr-1]" # TeX("\\text{R}_\\text{rs}")
          )),
          yaxis2 = list(title = list(
            text = "Rrs uncertainty [sr-1]" # TeX("\\text{K}_\\text{Lu}")
          )) # ,
          # xaxis3 = list(title = list(text = TeX("\\text{wavelength}")))
        ) %>%
        config(mathjax = "cdn", displayModeBar = T)

      ply$x$source <- "KLu"

      # Save graph
      # save_image(ply, file=file.path(path.expand("~"), "sear_figure", "AOPs.svg"), scale = 3, height = 720, width = 1280)

      widgetframe::frameableWidget(ply)
    })



    # Smooth KLu --------------------------------------------------------------
    output$SmoothKLu <- renderUI({
      validate(need(
        {
          Settings$HOCR$WaveMin() &
            Settings$HOCR$WaveMax() &
            Settings$HOCR$WaveStep() &
            Settings$HOCR$Z1Depth() &
            Settings$HOCR$Z1Z2Depth()
        },
        message = "Need KLu"
      ))

      actionButton(ns("SmoothKLu"), "Smooth KLu")
    })
  })
}

## To be copied in the UI
# mod_L1hocr_l2_ui("L1hocr_l2")

## To be copied in the server
# mod_L1hocr_l2_server("L1hocr_l2")
