#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinyjs sodium
#' @noRd

# Main login screen
LogInPage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600;
                                            padding-top: 5px;font-size:16px;",
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: myuser  Password: mypass"),
                     br(),
                     tags$code("Username: myuser1  Password: mypass1")
                   ))
)

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"),sodium::password_store),
  permission  = c("admin", "analyst"),
  stringsAsFactors = F
)

app_server <- function(input, output, session) {

  login = FALSE
  USER <- reactiveValues(login = login)

  observe({
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) {
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        }
      }
    }
  })

  output$LogOutBtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa-light fa-right-from-bracket"), "Logout",
              href="javascript:window.location.reload(true)"),
            class = "dropdown",
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })

  output$SideBar <- renderUI({
    if (USER$login == TRUE ){
      tagList(
        mod_manage_project_ui("manage_project"),
        sidebarMenu(
          menuItem("Settings", tabName = "Settings", icon = icon("wrench")),
          menuItem("Processing", tabName = "Processing", icon = icon("gears"))
        ),
        # mod_filter_trim_ui("filter_trim"),
        # mod_discretize_ui("discretize"),
        mod_L1b_process_ui("L1b_process"),
        mod_manage_db_ui("manage_db")#,
        # sidebarMenu(
        #   menuItem("DataBase", tabName = "DataBase", icon = icon("database"))
        # )
      )
    }
  })

  output$Body <- renderUI({
    if (USER$login == TRUE ) {
      tagList(
        tabItems(
          tabItem(tabName = "Settings",
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

          tabItem(tabName = "Processing",
                  fluidRow(
                    column(
                      width = 6,
                      mod_select_data_ui("select_data")
                    ),
                    column(
                      width = 6,
                      mod_L1L2_ui("L1L2")
                    )
                  )
          )
        )
      )

    }
    else {
      LogInPage
    }
  })

  #format(L1$SBE19()$DateTime, "%Y-%m-%d %H:%M:%OS3")
  options(digits.secs = 3)
  options(shiny.maxRequestSize=50*1024^2) # 50MB
  options(shiny.reactlog = TRUE)

  # Global objects ----------------------------------------------------------

  MainLog <- reactiveVal(tibble())

  # Active discrete observation
  Obs <- reactiveValues(
    Metadata = tibble(
      ObsName = character(),
      ObsType = character(),
      ObsFlag = character(),
      DateTime = character(),
      DateTimeMin = character(),
      DateTimeMax = character(),
      TimeElapsed = numeric(),
      Lat = numeric(),
      Lon = numeric(),
      LatMin = numeric(),
      LatMax = numeric(),
      LonMin = numeric(),
      LonMax = numeric(),
      Altitude = numeric(),
      DistanceRun = numeric(),
      BoatSolAzm = numeric(),
      Comment = character(),
      UUID = character()
    ),
    HOCR = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    SBE19 = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    SeaOWL = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    BBFL2 = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    ),
    BioSonic = reactiveValues(
      L1b = tibble(),
      L2 = tibble()
    )
  )


# Modules logic -----------------------------------------------------------

  SearTbl <- mod_manage_project_server("manage_project")

  CalData <- mod_parse_cal_server("parse_cal", SearTbl)

  DB <- mod_manage_db_server("manage_db", SearTbl, SelData, Obs)

  L1 <- mod_parse_data_server("parse_data", SearTbl, CalData, MainLog)

  SelData <- mod_select_data_server("select_data", MainLog, DB, Obs, ManObs, L1)

  L1b <- mod_L1b_process_server("L1b_process", L1, SelData, CalData, Obs)

  L2 <- mod_L1L2_server("L1L2", L1b, Obs)

  ManObs <- mod_manage_obs_server("manage_obs", DB, L2, SelData, Obs)
}
