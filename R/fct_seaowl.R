#' cal_seaowl
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
cal_seaowl <- function(SeaOWLData, SeaOWLCal) {

  # Test that calibration and data have the same serial number
  # Should be stop
  if (!all(SeaOWLCal$SERIALNO == stringr::str_extract(SeaOWLData$SN, "(?<=-).*"))) {
    warning("Calibration and SeaOWL have different serial number")
  }

  SeaOWLData %>%
    mutate(
      DateTime = as.character(DateTime),
      Bb_700 = SeaOWLCal$BbScaleFactor * (Bb700Output - SeaOWLCal$BbDarkCounts),
      Chl = SeaOWLCal$ChlScaleFactor * (ChlOutput - SeaOWLCal$ChlDarkCounts),
      FDOM = SeaOWLCal$FDOMScaleFactor * (FDOMOutput - SeaOWLCal$FDOMDarkCounts),
      .after = SN
    ) %>%
    select(DateTime, SN, Bb_700, Chl, FDOM)
}
