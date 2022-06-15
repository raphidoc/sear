#' project_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyFiles
#' @import shinyWidgets
#'
mod_manage_project_ui <- function(id){
  ns <- NS(id)
  tagList(

    dropdownButton(
      shinyDirButton(id = ns('Select'), label = 'Select', title = 'Open or create a project root folder', multiple = FALSE),
      circle = F,
      label = textOutput(ns("ProjectName")),
      tootltip = textOutput(ns("ProjectPath"))
      )

  )
}

#' project_manager Server Functions
#'
#' @noRd
mod_manage_project_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Server side function of shinyDirButton
    shinyDirChoose(input, id = 'Select', allowDirCreate = T, roots=c(Data='/D/Data',root='/'))

    # Store project Name and path in a reactive value
    Project <- reactive({

      # input$Select gets cumputed as soon as the button is clicked (before a folder is choosed),
      # this early computation leed to error as input$Select is first an integer then a list.
      req(is.list(input$Select))

      Path <- parseDirPath(roots=c(Data='/D/Data',root='/'), input$Select)

      Name <- str_extract(Path, "(?<=/)[^/]*$")

      list(
        Path = Path,
        Name = Name
      )
    })

    # Update UI element to display project name and path
    output$ProjectPath <- renderText({
      validate(need(is.list(input$Select), message = "Project: None"))

      Project()$Path
    })

    output$ProjectName <- renderText({
      validate(need(is.list(input$Select), message = "Project: None"))

      Project()$Name
    })

    # Check if .searproj file already exist if not create the default one

    SearTbl <- reactive({
      req(Project())

      searproj <- file.path(Project()$Path, ".searproj")

      if (file.exists(searproj)) {
        message("Reading ", searproj)

        SearTbl <- read_csv(searproj)

        SearTbl <- SearTbl %>%
          mutate(
            ProjPath = Project()$Path,
            Updated = Sys.time()
          )

        write_csv(SearTbl ,searproj)

        # return SearTble pbject
        SearTbl

      } else {
        message("Creating ", searproj)

        SearTbl <- tibble::tibble(
          ProjName = Project()$Name,
          Created = Sys.time(),
          ProjPath = Project()$Path
        )

        write_csv(SearTbl ,searproj)

        # return SearTble pbject
        SearTbl

      }
    })

  })
}

## To be copied in the UI
# mod_project_manager_ui("project_manager")

## To be copied in the server
# mod_project_manager_server("project_manager")
