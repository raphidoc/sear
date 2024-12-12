#' #' manage_user UI Function
#' #'
#' #' @description a shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_manage_user_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'
#'   )
#' }
#'
#' # Main login screen
#' LogInPage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
#'                  wellPanel(
#'                    tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
#'                    textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
#'                    passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock"), "Password")),
#'                    br(),
#'                    div(
#'                      style = "text-align: center;",
#'                      actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
#'                                  padding: 10px 15px; width: 150px; cursor: pointer;
#'                                  font-size: 18px; font-weight: 600;"),
#'                      actionButton("SignUp", "SIGN UP", style = "color: white; background-color:#3c8dbc;
#'                                  padding: 10px 15px; width: 150px; cursor: pointer;
#'                                  font-size: 18px; font-weight: 600;"),
#'                      shinyjs::hidden(
#'                        div(id = "nomatch",
#'                            tags$p("Oops! Incorrect username or password!",
#'                                   style = "color: red; font-weight: 600;
#'                                             padding-top: 5px;font-size:16px;",
#'                                   class = "text-center"))),
#'                      br(),
#'                      br(),
#'                      tags$code("Username: myuser  Password: mypass"),
#'                      br(),
#'                      tags$code("Username: myuser1  Password: mypass1")
#'                    ))
#' )
#'
#' credentials = data.frame(
#'   username_id = c("myuser", "myuser1"),
#'   passod   = sapply(c("mypass", "mypass1"),sodium::password_store),
#'   permission  = c("admin", "analyst"),
#'   stringsAsFactors = F
#' )
#'
#' #' manage_user Server Functions
#' #'
#' #' @noRd
#' mod_manage_user_server <- function(id){
#'   moduleServer( id, function(input, output, session){
#'     ns <- session$ns
#'
#'     login = FALSE
#'     USER <- reactiveValues(login = login)
#'
#'     observe({
#'       if (USER$login == FALSE) {
#'         if (!is.null(input$login)) {
#'           if (input$login > 0) {
#'             Username <- isolate(input$userName)
#'             Password <- isolate(input$passwd)
#'             if(length(which(credentials$username_id==Username))==1) {
#'               pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
#'               pasverify <- password_verify(pasmatch, Password)
#'               if(pasverify) {
#'                 USER$login <- TRUE
#'               } else {
#'                 shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
#'                 shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
#'               }
#'             } else {
#'               shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
#'               shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
#'             }
#'           }
#'         }
#'       }
#'     })
#'
#'     output$LogOutBtn <- renderUI({
#'       req(USER$login)
#'       tags$li(a(icon("fa-light fa-right-from-bracket"), "Logout",
#'                 href="javascript:window.location.reload(true)"),
#'               class = "dropdown",
#'               style = "background-color: #eee !important; border: 0;
#'                     font-weight: bold; margin:5px; padding: 10px;")
#'     })
#'
#'     SideBar <- renderUI({
#'       if (USER$login == TRUE ){
#'         tagList(
#'           mod_manage_project_ui("manage_project"),
#'           sidebarMenu(
#'             menuItem("Settings", tabName = "Settings", icon = icon("wrench")),
#'             menuItem("Processing", tabName = "Processing", icon = icon("gears"))
#'           ),
#'           # mod_filter_trim_ui("filter_trim"),
#'           # mod_discretize_ui("discretize"),
#'           mod_L1b_process_ui("L1b_process"),
#'           mod_manage_db_ui("manage_db")#,
#'           # sidebarMenu(
#'           #   menuItem("DataBase", tabName = "DataBase", icon = icon("database"))
#'           # )
#'         )
#'       }
#'     })
#'
#'     output$Body <- renderUI({
#'       if (USER$login == TRUE ) {
#'         tagList(
#'           tabItems(
#'             tabItem(tabName = "Settings",
#'                     fluidRow(
#'                       column(
#'                         width = 6,
#'                         mod_parse_data_ui("parse_data")
#'                       ),
#'                       column(
#'                         width = 6,
#'                         mod_parse_cal_ui("parse_cal")
#'                       )
#'                     )
#'             ),
#'
#'             tabItem(tabName = "Processing",
#'                     fluidRow(
#'                       column(
#'                         width = 6,
#'                         mod_select_data_ui("select_data")
#'                       ),
#'                       column(
#'                         width = 6,
#'                         mod_L1L2_ui("L1L2")
#'                       )
#'                     )
#'             )
#'           )
#'         )
#'
#'       }
#'       else {
#'         LogInPage
#'       }
#'     })
#'
#'
#' # Module export -----------------------------------------------------------
#'
#'     list(
#'       Sidebar = Sidebar,
#'       Body = Body
#'     )
#'
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_manage_user_ui("manage_user_1")
#'
#' ## To be copied in the server
#' # mod_manage_user_server("manage_user_1")
