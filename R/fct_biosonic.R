#' biosonic
#'
#' @description a fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_biosonic <- function(CSVFile) {
  BioSonic <- read_delim(
    CSVFile,
    delim = ","
  ) %>%
    rename(
      lon = Longitude_deg,
      lat = Latitude_deg,
      date_time = Time,
      altitude_m = Altitude_mReMsl,
      bottom_elevation_m = BottomElevation_m,
      plant_height_m = PlantHeight_m,
      percent_coverage = PercentCoverage
      ) %>%
    mutate(date_time = as.character(ymd_hms(date_time)))
}
