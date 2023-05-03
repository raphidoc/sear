#' project_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
#'
mod_manage_project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dropdownButton(
      actionGroupButtons(
        c(ns("New"), ns("Open")),
        labels=c("New project", "Open project"),
        direction = "vertical"
      ),
      circle = F,
      label = textOutput(ns("ProjectName")),
      tootltip = textOutput(ns("ProjectPath"))
    )
  )
}

#' project_manager Server Functions
#'
#' @noRd
mod_manage_project_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    Project <- reactiveValues(
      Path = "",
      Name = ""
    )

    ModalNew <- modalDialog(
      textInput(ns("NewProj"),"Enter a project name", value = "", placeholder = "super_duper"),
      title = "Creating new project",
      footer = tagList(
        actionButton(ns("create"), "Create"),
        modalButton("Cancel")
      )
    )

    observeEvent(input$New,
      {
        showModal(ModalNew)
      }
    )

    observeEvent(input$cancel,
      {
        removeModal()
      }
    )

    observeEvent(input$create,
      {
        message(getwd())

        user <- system2('echo', '"$USER"', stdout = T)

        message(paste0("Connected as: ", user))

        ProjPath <- normalizePath(file.path("~","sear_project",input$NewProj))

        dir.create(ProjPath, recursive = T)

        #setwd(ProjPath)

        #message(getwd())

        Project$Path <- ProjPath
        Project$Name <- basename(ProjPath)

        removeModal()
      }
    )

    ModalOpen <- modalDialog(
      selectizeInput(
        ns("ProjList"),
        "Select a project",
        choices = c("", list.files(
          normalizePath(file.path("~","sear_project")),
          full.names = T)),
        selected = NULL,
        multiple = F),
      title = "Opening an existing project",
      footer = tagList(
        modalButton("Cancel")
      )
    )

    observeEvent(
      ignoreInit = T,
      input$Open,
      {
        showModal(ModalOpen)
      }
    )

    test_input <- function(input){
      if (!is.null(input)) {
        if (input == "") {
          return(NULL)
        } else {
          T
        }
      }
    }

    observeEvent(
      ignoreInit = T,
      ignoreNULL = T,
      {test_input(input$ProjList)},
      {
        Project$Path <- input$ProjList
        Project$Name <- basename(input$ProjList)

        removeModal()
      }
    )

    # Update UI element to display project name and path
    output$ProjectPath <- renderText({
      validate(need(Project$Path != "", message = "Project: None"))

      Project$Path
    })

    output$ProjectName <- renderText({
      validate(need(Project$Name != "", message = "Project: None"))

      Project$Name
    })

    # Check if .searproj file already exist if not create the default one

    AccessUUID <- reactiveVal(uuid::UUIDgenerate(use.time = T, output = "string"))

    Con <- reactive({
      req(Project$Path)
      PotSQLite <- file.path(Project$Path, "sear.sqlite")
      DBI::dbConnect(RSQLite::SQLite(), PotSQLite, extended_types = TRUE)}
      )

    History <- reactive({
      req(Project$Path)

      message("Doing sear SQLite stuff ... you know")

      PotSQLite <- file.path(Project$Path, "sear.sqlite")

      if (file.exists(PotSQLite)) {
        message("Reading ", PotSQLite)

        SearProj <- tibble::tibble(
          DateTime = as.character(Sys.time()),
          User = system2("echo","$USER", stdout = T),
          SearVersion = as.character(packageVersion("sear")),
          ProjName = Project$Name,
          ProjPath = Project$Path,
          UUID = AccessUUID()
        )

        DBI::dbWriteTable(Con(), "History", SearProj, append = TRUE)

        # return SearTble object
        SearProj

      } else {
        message("Creating ", PotSQLite)

        #Con <- DBI::dbConnect(RSQLite::SQLite(), PotSQLite, extended_types = TRUE)

        # Enable foreign keys
        DBI::dbExecute(conn = Con, "PRAGMA foreign_keys=ON")

        # Create DB schema
        # Acces history
        DBI::dbSendStatement(
          Con,
          "CREATE TABLE IF NOT EXISTS History (
          DateTime TEXT NOT NULL,
          User TEXT NOT NULL,
          SearVersion TEXT NOT NULL,
          ProjName TEXT NOT NULL,
          ProjPath TEXT NOT NULL,
          UUID TEXT PRIMARY KEY
          );"
        )

        SearProj <- tibble::tibble(
          DateTime = as.character(Sys.time()),
          User = system2("echo","$USER", stdout = T),
          SearVersion = as.character(packageVersion("sear")),
          ProjName = Project$Name,
          ProjPath = Project$Path,
          UUID = AccessUUID()
        )

        DBI::dbWriteTable(Con(), "History", SearProj, append = TRUE)

        # return SearTble object
        SearProj
      }

    })

# Module output -----------------------------------------------------------

    list(
      History = History,
      Con = Con,
      AccessUUID = AccessUUID
    )

  })
}

## To be copied in the UI
# mod_project_manager_ui("project_manager")

## To be copied in the server
# mod_project_manager_server("project_manager")
