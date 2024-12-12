#' cal_seaowl
#'
#' @description a fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
cal_seaowl <- function(SeaOWLData, SeaOWLCal) {
  # Test that calibration and data have the same serial number
  # Should be stop
  sn <- stringr::str_extract(SeaOWLData$sn, "(?<=-).*")
  sn <- unique(sn[!is.na(sn)])
  if (!SeaOWLCal$SERIALNO == sn) {
    warning("Calibration and SeaOWL have different serial number")
  }

  SeaOWLData %>%
    mutate(
      date_time = as.character(date_time),
      vsf_700 = SeaOWLCal$vsf_scale_factor * (vsf_700_channel - SeaOWLCal$vsf_dark_count),
      chl = SeaOWLCal$chl_scale_factor * (chl_channel - SeaOWLCal$chl_dark_count),
      fdom = SeaOWLCal$fdom_scale_factor * (fdom_output - SeaOWLCal$fdom_dark_counts),
      .after = sn
    ) %>%
    select(date_time, sn, vsf_700, chl, fdom)
}
