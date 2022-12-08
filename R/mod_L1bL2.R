#' L1L2_obs UI Function
#'
#' @description A shiny Module.
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
mod_L1bL2_server <- function(id, Obs) {
  # stopifnot(is.reactive(L1b$Data))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tab panel ---------------------------------------------------------------
    output$TabPanel <- renderUI({
      req(nrow(Obs$Metadata) != 0)

      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
        tabPanel(
          "Observation",
          uiOutput(ns("ObsTab"))
        ),
        tabPanel(
          "HOCR",
          mod_L1L2_hocr_ui(ns("L1L2_hocr"))
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
    output$DataTable <- DT::renderDataTable(
      DT::datatable(Obs$Metadata,
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
              targets = c(0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12)
            )
          ),
          deferRender = TRUE,
          scrollY = 100,
          pageLength = 10,
          scroller = TRUE
        ),
        selection = "none",
        editable = F
      ),
      server = FALSE,
      editable = F
    )

    output$ObsTab <- renderUI({
      tagList(
        DT::DTOutput(ns("DataTable")),
        textAreaInput(
          ns("Comment"),
          "Comment",
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
        Obs$Metadata <- Obs$Metadata %>% mutate(Comment = input$Comment)
      }
    )

    # HOCR tab ----------------------------------------------------------------

    mod_L1L2_hocr_server("L1L2_hocr", Obs)

    # SBE19 tab ---------------------------------------------------------------

    mod_L1L2_sbe19_server("L1L2_sbe19", Obs)

    # SeaOWL tab --------------------------------------------------------------

    mod_L1L2_seaowl_server("L1L2_seaowl", Obs)

    # BBFL2 tab ---------------------------------------------------------------

    mod_L1L2_bbfl2_server("L1L2_bbfl2", Obs)

    # BioSonic ----------------------------------------------------------------

    mod_L1L2_biosonic_server("L1L2_biosonic", Obs)


# Module output -----------------------------------------------------------

    list(
      # Save = reactive(input$Save),
      # Delete = reactive(input$Delete)
      # ObsTbl = ObsTbl,
      # HOCR = HOCRL2
    )
  })
}

## To be copied in the UI
# mod_L1bL2_ui("L1bL2")

## To be copied in the server
# mod_L1bL2_server("L1bL2")
