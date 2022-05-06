#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Max download size allowed in bytes
  options(shiny.maxRequestSize = 10000*1024^2)

  DataFiles <- mod_load_mtelog_server("load_mtelog")

  Data <- mod_parse_mtelog_server("parse_mtelog", DataFiles)

  SelDisplay <- mod_selection_display_server("selection_display", Data$Apla)

  mod_select_instrument_server("select_instrument", Data$MainLog)

  CalData <- mod_load_cal_server("HOCRCal")

  L2Data <- mod_process_L1L2_server("process_L1L2", Data$Apla, SelDisplay$UpApla, SelDisplay$Selected, Data$RawHOCR, Data$TimeIndexHOCR, CalData)

  mod_L1L2_station_server("L1L2_station", L2Data)

}
