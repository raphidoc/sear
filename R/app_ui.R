#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard plotly
#' @noRd
app_ui <- function(request) {
  Header <- dashboardHeader(
    title = tags$a(
      id = "app-name",
      href = "https://github.com/raphidoc/sear",
      target = "_blank",
      rel = "noopener noreferrer",
      tags$img(src = "www/logo_sear_2.png", width = "100px", height = "40px"),
      "sear"
    )
  )

  Sidebar <- dashboardSidebar(
    mod_manage_project_ui("manage_project"),
    sidebarMenu(
      id = "ActiveMenu",
      menuItem("Settings", tabName = "Settings", icon = icon("wrench")),
      menuItem("Parsing", tabName = "Parsing", icon = icon("fa-regular fa-file-lines")),
      menuItem("Processing", tabName = "Processing", icon = icon("gears"))
    ),
    fluidRow(
      mod_L1b_process_ui("L1b_process"),
      mod_automatic_processing_ui("automatic_processing")
    ),
    mod_filter_trim_ui("filter_trim"),
    # mod_discretize_ui("discretize"),
    sidebarMenu(
      menuItem("DataBase", tabName = "Database", icon = icon("database"))
    ),
    mod_manage_db_ui("manage_db")
  )

  Body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "Settings",
        mod_settings_ui("settings")
      ),
      tabItem(
        tabName = "Parsing",
        fluidRow(
          column(
            width = 6,
            mod_parse_data_ui("parse_data")
          ),
          column(
            width = 6,
            mod_parse_cal_ui("parse_cal")
          )
        )
      ),
      tabItem(
        tabName = "Processing",
        fluidRow(
          column(
            width = 6,
            mod_L1a_select_ui("L1a_select")
          ),
          column(
            width = 6,
            mod_L1bL2_ui("L1bL2")
          )
        )
      ),
      tabItem(
        tabName = "Database",
        mod_L2_select_ui("L2_select")
      )
    )
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    dashboardPage(Header, Sidebar, Body)
  )
}

# app_ui <- shinymanager::secure_app(app_ui, enable_admin = TRUE)

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(
      ico = "hex_sear",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sear"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

# Taken from https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
# Takes a location 'href', an image location 'src', a loading gif 'loadingsrc'
# height, width and alt text, and produces a loading logo that activates while
# Shiny is busy.
loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)"
      )
    ),
    tags$a(
      href = href,
      div(
        class = "busy",
        img(src = loadingsrc, height = height, width = width, alt = alt)
      ),
      div(
        class = "notbusy",
        img(src = src, height = height, width = width, alt = alt)
      )
    )
  )
}
