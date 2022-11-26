#' biosonic
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_biosonic <- function(CSVFile) {

  BioSonic <- read_delim(
    CSVFile,
    delim = ";"
    ) %>%
    rename(DateTime = Time) %>%
    mutate(DateTime = ymd_hms(DateTime))

}
