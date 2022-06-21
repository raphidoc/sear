#' manage_DB UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manage_DB_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("ObsList"))
  )
}

#' manage_DB Server Functions
#'
#' @noRd
mod_manage_DB_server <- function(id, SearTbl, SelData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # DBObs <- reactiveValues(
    #   UUID = {list()},
    #   Tbl = {tibble()}
    # )

    #Con <- reactiveVal()

# Connect project SQLite DB -----------------------------------------------
    Con <- eventReactive(
      req(SearTbl()$ProjPath),
      {

        L2dir <- file.path(SearTbl()$ProjPath,"L2")

        PotSQLite <- file.path(L2dir, paste0(SearTbl()$ProjName,"_sear.sqlite"))

        if (!dir.exists(L2dir)) {
          dir.create(L2dir)
        }

        Con <- DBI::dbConnect(RSQLite::SQLite(), PotSQLite)

        # if DB is not empty list UUID and get current index
        # if (
        #   !identical(DBI::dbListTables(Con()), character(0)) #&
        #   #str_detect(DBI::dbListTables(Con), "ObsMeta")
        # ) {
        #   message("Listing Obs")
        #
        #   browser()
        #
        #   DBObs(DBI::dbGetQuery(Con(), "SELECT UUID FROM ObsMeta"))
        # }

      })

    # observeEvent(
    #   req(SearTbl()$ProjPath),
    #   {
    #     Con()
    #   })



# Fetch MetaData ----------------------------------------------------------

    ObsMeta <- reactive({
      req(Con())

      tibble(DBI::dbGetQuery(Con(), "SELECT * FROM Metadata"))

    })

    #Obs = {reactive(add_trace)}




    output$ObsList <- renderUI({

      validate(need(ObsMeta, label = "Empty DB"))

      selectInput(ns("ObsList"), "ObsList", choices = ObsMeta()$UUID, selected = NULL, multiple = F)
    })

    list(
      ObsMeta = ObsMeta,
      ObsSel = reactive(input$ObsList),
      Con = Con
    )


  })
}

## To be copied in the UI
# mod_manage_DB_ui("manage_DB")

## To be copied in the server
# mod_manage_DB_server("manage_DB")
