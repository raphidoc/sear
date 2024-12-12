#' oxy_sol
#'
#' @description oxygen solubility in ml/l from Garcia and Gordon 1992
#' (taken from sbe43 application note 64, revision june 2013, Appendix a)
#'
#' @return oxygen solubility in ml/l
#'
#' @noRd

oxy_sol <- function(temperature, salinity_practical) {
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

  Ts <- log((298.15 - temperature) / (273.15 + temperature))

  oxygen_solubility <- exp({
    A0 + (A1 * Ts) + (A2 * Ts)^2 + (A3 * Ts)^3 + (A4 * Ts)^4 + (A5 * Ts)^5 +
      salinity_practical * (B0 + (B1 * Ts) + (B2 * Ts)^2 + (B3 * Ts)^3) + (C0 * salinity_practical)^2
  })
}

#' cal_sbe19
#'
#' @description add gsw (TEOS-10) variables which can be computed from CTD only
#'
#' @return same data frame with added variables
#'
#' @noRd

cal_sbe19 <- function(Data, lon, lat) {
  Data %>%
    mutate(
      date_time = as.character(date_time),
      z = gsw::gsw_z_from_p(
        p = pressure,
        latitude = lat
      ),
      salinity_practical = gsw::gsw_SP_from_C( # Salinity Practical in PSU
        C = conductivity * 10,
        t = temperature,
        p = pressure
      ),
      salinity_absolute = gsw::gsw_SA_from_SP( # Salinity Absolute in g/Kg
        SP = salinity_practical,
        p = pressure,
        long = lon,
        lat = lat
      ),
      conservative_temperature = gsw::gsw_CT_from_t( # Conservative temperature in deg c
        SA = salinity_absolute,
        t = temperature,
        p = pressure
      ),
      oxygen_solubility = gsw::gsw_O2sol( # oxygen_concentration solubility in umol/kg
        SA = salinity_absolute,
        CT = conservative_temperature,
        p = pressure,
        longitude = lon,
        latitude = lat
      ),
      oxygen_solubility_g92 = oxy_sol( # oxygen solubility in ml/l from Garcia and Gordon 1992
        temperature = temperature,
        salinity_practical = salinity_practical
      )
    )
}

#' cal_sbe43
#'
#' @description compute oxygen
#'
#' @return oxygen_concentration in ml/l multiply by 1.42903 to get mg/l
#'
#' @noRd

cal_sbe43 <- function(Volt, Tcelsius, pressure, oxygen_solubility, cal_data) {
  soc <- cal_data$soc
  voffset <- cal_data$voffset
  a <- cal_data$a
  b <- cal_data$b
  c <- cal_data$c
  e <- cal_data$e

  Tkelvin <- Tcelsius + 273.15

  oxygen_concentration <- (soc * (Volt + voffset)) * oxygen_solubility * (1.0 + a * Tcelsius + b * Tcelsius^2 + c * Tcelsius^3) * exp(1)^(e * pressure / Tkelvin)
}

#' cal_sbe18
#'
#' @description compute ph
#'
#' @return ph
#'
#' @noRd

cal_sbe18 <- function(Volt, Tcelsius, cal_data) {
  voffset <- cal_data$offset
  slope <- cal_data$slope

  Tkelvin <- Tcelsius + 273.15

  ph <- 7.0 + (Volt - voffset) / (slope * Tkelvin * 1.98416e-4)
}
