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
      dashboardHeader(
        title = tags$a(
          href = "https://github.com/raphidoc/sear",
          tags$img(src = "www/logo_sear.png", width = "100px", height = "40px"),
          "seaR"
        )
      ),
      dashboardSidebar(
        mod_manage_project_ui("manage_project"),
        dropdownButton(
          mod_load_mtelog_ui("load_mtelog"),
          circle = F,
          label = "Load data",
          tootltip = "path to go here"
        ),
        # mod_filter_trim_ui("filter_trim"),
        # mod_discretize_ui("discretize"),
        mod_process_L1b_ui("process_L1b"),
        mod_manage_DB_ui("manage_DB")
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
            mod_obs_L1L2_ui("obs_L1L2")
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
    favicon(
      ico = "hex_sear",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sear"
    )
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
