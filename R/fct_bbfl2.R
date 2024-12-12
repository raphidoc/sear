#' cal_bbfl2
#'
#' @description a fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
cal_bbfl2 <- function(BBFl2_data, BBFL2Cal) {
  BBFl2_data %>%
    mutate(
      date_time = as.character(date_time),
      ntu = BBFL2Cal$ntu_scale_factor * (ntu_channel - BBFL2Cal$ntu_dark_count),
      pe = BBFL2Cal$pe_scale_factor * (pe_channel - BBFL2Cal$pe_dark_count),
      pc = BBFL2Cal$pc_scale_factor * (pc_channel - BBFL2Cal$pc_dark_count)
    ) %>%
    select(date_time, ntu, pe, pc)
}
