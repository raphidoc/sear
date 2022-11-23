#' cal_bbfl2
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
cal_bbfl2 <- function(BBFL2Data, BBFL2Cal) {

  BBFL2Data %>%
    mutate(
      NTU = BBFL2Cal$NTUScaleFactor * (NTUSig - BBFL2Cal$NTUDarkCounts),
      PE = BBFL2Cal$PEScaleFactor * (PESig - BBFL2Cal$PEDarkCounts),
      PC = BBFL2Cal$PCScaleFactor * (PCSig - BBFL2Cal$PCDarkCounts)
    ) %>%
    select(DateTime, NTU, PE, PC)
}
