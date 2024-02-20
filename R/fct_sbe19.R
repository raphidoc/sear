#' oxy_sol
#'
#' @description oxygen solubility in ml/l from Garcia and Gordon 1992
#' (taken from sbe43 application note 64, revision june 2013, Appendix A)
#'
#' @return oxygen solubility in ml/l
#'
#' @noRd

oxy_sol <- function(Temperature, SP) {
  A0 <- 2.00907
  A1 <- 3.22014
  A2 <- 4.0501
  A3 <- 4.94457
  A4 <- -0.256847
  A5 <- 3.88767
  B0 <- -0.00624523
  B1 <- -0.00737614
  B2 <- -0.010341
  B3 <- -0.00817083
  C0 <- -0.000000488682

  Ts <- log((298.15 - Temperature) / (273.15 + Temperature))

  OxSol <- exp({
    A0 + (A1 * Ts) + (A2 * Ts)^2 + (A3 * Ts)^3 + (A4 * Ts)^4 + (A5 * Ts)^5 +
      SP * (B0 + (B1 * Ts) + (B2 * Ts)^2 + (B3 * Ts)^3) + (C0 * SP)^2
  })
}

#' cal_sbe19
#'
#' @description add gsw (TEOS-10) variables which can be computed from CTD only
#'
#' @return same data frame with added variables
#'
#' @noRd

cal_sbe19 <- function(Data, Lon, Lat) {
  Data %>%
    mutate(
      DateTime = as.character(DateTime),
      SP = gsw::gsw_SP_from_C( # Salinity Practical in PSU
        C = Conductivity * 10,
        t = Temperature,
        p = Pressure
      ),
      SA = gsw::gsw_SA_from_SP( # Salinity Absolute in g/Kg
        SP = SP,
        p = Pressure,
        long = Lon,
        lat = Lat
      ),
      CT = gsw::gsw_CT_from_t( # Conservative Temperature in deg C
        SA = SA,
        t = Temperature,
        p = Pressure
      ),
      O2Sol = gsw::gsw_O2sol( # Oxygen solubility in umol/kg
        SA = SA,
        CT = CT,
        p = Pressure,
        longitude = Lon,
        latitude = Lat
      ),
      OxSol = oxy_sol( # oxygen solubility in ml/l from Garcia and Gordon 1992
        Temperature = Temperature,
        SP = SP
      )
    )
}

#' cal_sbe43
#'
#' @description compute oxygen
#'
#' @return Oxygen in ml/l multiply by 1.42903 to get mg/l
#'
#' @noRd

cal_sbe43 <- function(Volt, Tcelsius, Pressure, OxSol, CalData) {
  Soc <- CalData$SOC
  Voffset <- CalData$VOFFSET
  A <- CalData$A
  B <- CalData$B
  C <- CalData$C
  E <- CalData$E

  Tkelvin <- Tcelsius + 273.15

  Oxygen <- (Soc * (Volt + Voffset)) * OxSol * (1.0 + A * Tcelsius + B * Tcelsius^2 + C * Tcelsius^3) * exp(1)^(E * Pressure / Tkelvin)
}

#' cal_sbe18
#'
#' @description compute pH
#'
#' @return pH
#'
#' @noRd

cal_sbe18 <- function(Volt, Tcelsius, CalData) {
  Voffset <- CalData$OFFSET
  Slope <- CalData$SLOPE

  Tkelvin <- Tcelsius + 273.15

  pH <- 7.0 + (Volt - Voffset) / (Slope * Tkelvin * 1.98416e-4)
}
