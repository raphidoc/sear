#' L1L2_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_station_L1L2_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("TabPanel"))
  )
}

#' L1L2_station Server Functions
#'
#' @noRd
mod_station_L1L2_server <- function(id, L1b){

  stopifnot(is.reactive(L1b$Data))

  moduleServer( id, function(input, output, session){
    ns <- session$ns



# Tab panel ---------------------------------------------------------------
    output$TabPanel <- renderUI({
      req(L1b$Data())



      tabsetPanel(
        id = ns("Tabset"),
        type = "pills",
        tabPanel(
          "Station",
          uiOutput(ns("Station"))
        ),
        tabPanel("HOCR", mod_station_hocr_ui(ns("station_hocr")))
      )



    })


    # Keep that for dev purpose

    # observeEvent(
    #   req(StationTbl()),
    #   {
    #     appendTab(inputId ="Tabset", tabPanel("Station", uiOutput(ns("Station"))))
    #   })
    #
    # observeEvent(
    #   req(str_detect(names(L1b$Data()), "HOCR")),
    #   {
    #     browser()
    #     appendTab(inputId ="Tabset", tabPanel("HOCR", mod_station_hocr_ui(ns("station_hocr"))))
    #   })

# Station tab -------------------------------------------------------------
    StationTbl <- reactiveVal({})

    observeEvent(
      L1b$ProcessL1b(),
      {
        StationTbl(
          tibble(
            ObsName = L1b$ObsName(),
            ObsType = L1b$ObsType(), # Obviously should be "Station"
            DateTime = mean(L1b$SelApla()$DateTime, na.rm = T),
            Lat = mean(L1b$SelApla()$Lat_DD, na.rm = T),
            Lon = mean(L1b$SelApla()$Lon_DD, na.rm = T)
          )
        )
      }
    )

    #DataTable used to display Station information
    output$DataTable <- DT::renderDataTable(
      DT::datatable(StationTbl(),
                    #extensions = c("Buttons", "Scroller", "Select"),
                    #filter = "top",
                    escape = TRUE, rownames = FALSE,
                    style = "bootstrap",
                    class = "compact",
                    options = list(
                      #dom = "Brtip",
                      #select = list(style = 'os', items = 'row'),
                      #buttons = list(I("colvis"),"selectNone","csv"),
                      #columnDefs = list(
                       # list(
                          #visible = FALSE,
                          #targets = c(0,2,3)
                        #)),
                      deferRender = TRUE,
                      scrollY = 100,
                      pageLength = 10,
                      scroller = TRUE
                    ),
                    selection = "none",
                    editable = F
      ),
      server=FALSE,
      editable=T
    )

    output$Station <- renderUI({

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

        actionButton(ns("Delete"), "Delete", icon = icon("glyphicon glyphicon-trash", lib = "glyphicon")),
        actionButton(ns("Save"), "Save", icon = icon("glyphicon glyphicon-save", lib = "glyphicon"))
      )
    })

    observeEvent(
      input$Save,
      {
        StationTbl(StationTbl() %>% mutate(Comment = input$Comment))
      })

# HOCR tab ----------------------------------------------------------------

    mod_station_hocr_server("station_hocr", L1b$Data)

# BB3 tab -----------------------------------------------------------------



   })
}

## To be copied in the UI
# mod_L1L2_station_ui("L1L2_station")

## To be copied in the server
# mod_L1L2_station_server("L1L2_station")
