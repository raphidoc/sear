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
