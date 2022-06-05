#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard plotly
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        mod_project_manager_ui("project_manager"),
        dropdownButton(
          mod_load_mtelog_ui("load_mtelog"),
          circle = F,
          label = "Load data",
          tootltip = "path to go here"
        ),
        mod_process_L1b_ui("process_L1b")
      ),
      dashboardBody(
        mod_parse_mtelog_ui("parse_mtelog"),
        fluidRow(
          column(
            width = 6,
            mod_select_data_ui("select_data")
          ),
          column(
            width = 6,
            mod_L1L2_station_ui("L1L2_station")
          )
        )
      )
    )
  )
}

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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sear"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
