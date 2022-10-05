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

<<<<<<< HEAD
        # Create DB schema

        DBI::dbGetQuery(
          Con, "CREATE TABLE IF NOT EXISTS Metadata (

          );"
          )
=======
        # Enable foreign keys
        DBI::dbExecute(conn = Con, "PRAGMA foreign_keys=ON")

        # Create DB schema
        # Metadata
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS Metadata (
          ObsName TEXT NOT NULL,
          ObsType TEXT NOT NULL,
          DateTime TEXT NOT NULL,
          Lat DOUBLE NOT NULL,
          Lon DOUBLE NOT NULL,
          Comment TEXT,
          UUID TEXT PRIMARY KEY,
          ProTime TEXT NOT NULL,
          Analyst TEXT NOT NULL,
          Mail TEXT NOT NULL
          );"
        )

        # HOCRL1b
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HOCRL1b` (
          `Instrument` TEXT,
          `SN` TEXT,
          `DateTime` TEXT,
          `ID` INTEGER,
          `QC` TEXT,
          `Type` TEXT,
          `Wavelength` REAL,
          `Channels` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )

        # HOCRL2
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS `HOCRL2` (
          `Wavelength` REAL,
          `Rrs` REAL,
          `KLu` REAL,
          `UUID` TEXT,
          FOREIGN KEY (UUID)
            REFERENCES Metadata (UUID)
            ON DELETE CASCADE
          )"
        )
>>>>>>> SQLite

        # Return Con
        Con

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

    ObsMeta <- reactiveVal({
      reactive({
        req(Con())

<<<<<<< HEAD
      tibble(DBI::dbGetQuery(Con(), "SELECT * FROM Metadata"))
=======
        # if DB is not empty list UUID and get current index
        if (
          !identical(DBI::dbListTables(Con()), character(0)) #&
          #str_detect(DBI::dbListTables(Con), "ObsMeta")
        ) {
          message("Listing Obs")
>>>>>>> SQLite

          tibble(DBI::dbGetQuery(Con(), "SELECT * FROM Metadata"))

        } else {
          tibble(
            ObsType = NA,
            ObsName = NA,
            UUID = NA,
            Lat = NA,
            Lon = NA,
            DateTime = NA
          )
        }
      })
    })



    #Obs = {reactive(add_trace)}

    output$ObsList <- renderUI({

      validate(need(ObsMeta(), label = "Empty DB"))

      selectInput(ns("ObsList"), "ObsList", choices = ObsMeta()()$UUID, selected = NULL, multiple = F)
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
