#' L1L2_obs UI Function
#'
#' @description a shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_L1bL2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("TabPanel"))
  )
}

#' L1L2_obs Server Functions
#'
#' @noRd
mod_L1bL2_server <- function(id, Obs, Settings) {
  # stopifnot(is.reactive(L1b$Data))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tab panel ---------------------------------------------------------------
    output$TabPanel <- renderUI({
      req(nrow(Obs$metadata_l2) != 0)

      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
        tabPanel(
          "Observation",
          uiOutput(ns("ObsTab"))
        ),
        tabPanel(
          "HOCR",
          mod_L1hocr_l2_ui(ns("L1hocr_l2"))
        ),
        tabPanel(
          "SBE19",
          mod_L1L2_sbe19_ui(ns("L1L2_sbe19"))
        ),
        tabPanel(
          "SeaOWL",
          mod_L1L2_seaowl_ui(ns("L1L2_seaowl"))
        ),
        tabPanel(
          "BBFL2",
          mod_L1L2_bbfl2_ui(ns("L1L2_bbfl2"))
        ),
        tabPanel(
          "BioSonic",
          mod_L1L2_biosonic_ui(ns("L1L2_biosonic"))
        ),
        tabPanel(
          "HydroBall",
          mod_L1L2_hydroball_ui(ns("L1L2_hydroball"))
        ),
        tabPanel(
          "Rb",
          mod_bottom_reflectance_ui(ns("bottom_reflectance"))
        )
      )
    })

    # Keep that for dev purpose

    # observeEvent(
    #   req(ObsTbl()),
    #   {
    #     appendTab(inputId ="Tabset", tabPanel("Obs", uiOutput(ns("Obs"))))
    #   })
    #
    # observeEvent(
    #   req(str_detect(names(L1b$Data()), "HOCR")),
    #   {
    #     appendTab(inputId ="Tabset", tabPanel("HOCR", mod_obs_hocr_ui(ns("obs_hocr"))))
    #   })

    # Obs tab -------------------------------------------------------------

    # DataTable used to display Obs information
    output$DataTable <- DT::renderDT(
      DT::datatable(
        Obs$metadata_l2,
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
              targets = str_which(colnames(Obs$metadata_l2), "\\b(?!Speed|distance_run|sol_zen|boat_raa|roll|pitch)\\b\\S+") - 1
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
        DT::formatRound(
          columns = as.character(na.omit(str_extract(colnames(Obs$metadata_l2), "Speed|distance_run|sol_zen|boat_raa|roll|pitch"))),
          digits = 2
        ),
      server = FALSE,
      editable = F
    )

    output$ObsTab <- renderUI({
      tagList(
        DT::DTOutput(ns("DataTable")),
        textAreaInput(
          ns("comment"),
          "comment",
          value = "No comment",
          width = NULL,
          height = NULL,
          cols = NULL,
          rows = NULL,
          placeholder = NULL,
          resize = NULL
        ),
        mod_manage_obs_ui("manage_obs")
      )
    })

    observeEvent(
      input$Save,
      {
        Obs$metadata_l2 <- Obs$metadata_l2 %>% mutate(comment = input$comment)
      }
    )

    # HOCR tab ----------------------------------------------------------------

    mod_L1hocr_l2_server("L1hocr_l2", Obs, Settings)

    # SBE19 tab ---------------------------------------------------------------

    mod_L1L2_sbe19_server("L1L2_sbe19", Obs)

    # SeaOWL tab --------------------------------------------------------------

    mod_L1L2_seaowl_server("L1L2_seaowl", Obs)

    # BBFL2 tab ---------------------------------------------------------------

    mod_L1L2_bbfl2_server("L1L2_bbfl2", Obs)

    # BioSonic ----------------------------------------------------------------

    mod_L1L2_biosonic_server("L1L2_biosonic", Obs)


    # HydroBall ---------------------------------------------------------------

    mod_L1L2_hydroball_server("L1L2_hydroball", Obs)

    # Bottom Reflectance ------------------------------------------------------

    mod_bottom_reflectance_server("bottom_reflectance", Obs)


    # Module output -----------------------------------------------------------

    list(
      # Save = reactive(input$Save),
      # Delete = reactive(input$Delete)
      # ObsTbl = ObsTbl,
      # HOCR = hocr_l2
    )
  })
}

## To be copied in the UI
# mod_L1bL2_ui("L1bL2")

## To be copied in the server
# mod_L1bL2_server("L1bL2")
