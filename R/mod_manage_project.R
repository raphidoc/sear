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

    observeEvent(
      input$New,
      {
        showModal(ModalNew)
      }
    )

    ModalNew <- modalDialog(
      textInput(ns("NewProj"),"Enter a project name", value = "", placeholder = "super_duper"),
      title = "Creating new project",
      footer = tagList(
        actionButton(ns("create"), "Create"),
        actionButton(ns("cancel"), "Cancel")
      )
    )

    observeEvent(input$cancel,
      {
        removeModal()
      }
    )

    observeEvent(
      input$create,
      {
        stop(getwd())

        #dir.create("input$NewProj")
      }
    )

    ModalOpen <- modalDialog(
      selectizeInput(ns("ProjList"), "Select a project", choices = "", selected = NULL, multiple = F),
      title = "Opening an existing project",
      footer = tagList(
        actionButton(ns("cancel"), "Cancel")
      )
    )

    # Store project Name and path in a reactive value
    Project <- reactive({
    })

    # Update UI element to display project name and path
    output$ProjectPath <- renderText({
      validate(need(is.list(input$S), message = "Project: None"))

      Project()$Path
    })

    output$ProjectName <- renderText({
      validate(need(is.list(input$Select), message = "Project: None"))

      Project()$Name
    })

    # Check if .searproj file already exist if not create the default one

    SearTbl <- reactive({
      req(Project())

      searproj <- file.path(Project()$Path, glue::glue(Project()$Name, ".searproj"))

      if (file.exists(searproj)) {
        message("Reading ", searproj)

        SearTbl <- read_csv(searproj)

        SearTbl <- SearTbl %>%
          mutate(
            ProjPath = Project()$Path,
            Updated = Sys.time()
          )

        write_csv(SearTbl, searproj)

        # return SearTble pbject
        SearTbl

      } else {
        message("Creating ", searproj)

        SearTbl <- tibble::tibble(
          ProjName = Project()$Name,
          Created = Sys.time(),
          ProjPath = Project()$Path
        )

        write_csv(SearTbl, searproj)

        # return SearTble object
        SearTbl
      }

    })

  })
}

## To be copied in the UI
# mod_project_manager_ui("project_manager")

## To be copied in the server
# mod_project_manager_server("project_manager")
