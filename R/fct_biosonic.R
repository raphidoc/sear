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
    rename(date_time = time) %>%
    mutate(date_time = as.character(ymd_hms(date_time)))
}
