#' solar_position
#'
#' @description wrapper function to get sun azimuth and altitude
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import suncalc
#'

possol <- function(){
  getSunlightPosition(date = NULL, lat = NULL, lon = NULL,
                      data = NULL, keep = c("altitude", "azimuth"))

}
