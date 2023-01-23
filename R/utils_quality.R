#' qc_shift
#'
#' @description shift selected ID between 0 and 1
#'
#' @return The original data frame with the selected ID QC value changed
#'
#' @noRd
# qc_shift <- function(df, Selected) {
#   df %>%
#     filter(ID == Selected) %>%
#     mutate(QC = if_else(QC == "1", "0", "1")) %>%
#     bind_rows(df %>% filter(ID != Selected))
# }

qc_shift <- function(df, Selected) {

  df %>%
    mutate(
      QC = case_when(
        ID != Selected ~ QC,
        QC == "1" ~ "0",
        QC == "0" ~ "1"
      )
    )
}

#' data_synthesis
#'
#' @description Create a data synthesis from two time vectors. Give information on which data have been acquired when.
#'
#' @param x time vector of the 'Main' log, usaly GNSS data).
#'
#' @param y time vector of the instrument for which to create the synthesis
#'
#' @param tol allowed time difference (+-)
#'
#' @return A logical vector (T/F), TRUE if
#'
#' @export

data_synthesis <- function(x, y, tol = 3) {

  purrr::map_lgl(
    .x = x,
    ~ any(near(as.numeric(.x), as.numeric(y), tol = tol))
  )
}
