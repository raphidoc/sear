#' water_optical_proprieties
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
compute_klu <- function(wavelength, luz1, luz2, z2z1 = 0.15) {

  klu_samples <- (log(luz1) - log(luz2)) / z2z1
  # klu_samples <- log(luz1/luz2) / z2z1

  klu_stats <- purrr::map_dfr(
    seq_along(wavelength), ~ {
      tibble(
        klu_median = median(klu_samples[, .x], na.rm = T),
        klu_mad = mad(klu_samples[, .x], na.rm = T)
      )
    })

  return(list(tibble(wavelength, klu_stats), klu_samples))
}

compute_lw <- function(wavelength, luz1, klu, z1, salinity, temperature) {

  tl = water_air_radiance_trans(salinity, temperature, wavelength)

  lw_samples <- tl * luz1 * exp(klu * abs(z1))

  lw_stats <- purrr::map_dfr(
    seq_along(wavelength), ~ {
      tibble(
        lw_median = median(lw_samples[, .x], na.rm = T),
        lw_mad = mad(lw_samples[, .x], na.rm = T)
      )
    })

  return(list(tibble(wavelength, lw_stats), lw_samples))
}

compute_rrs <- function(wavelength, lw, es) {

  rrs_samples <- lw/es

  rrs_stats <- purrr::map_dfr(
    seq_along(wavelength), ~ {
      tibble(
        rrs_median = median(rrs_samples[, .x], na.rm = T),
        rrs_mad = mad(rrs_samples[, .x], na.rm = T)
      )
    })

  return(tibble(wavelength, rrs_stats))
}

water_refractive_index <- function(salinity, temperature, wavelength) {
  # Quan, X. and Fry, E.S. (1995) ‘Empirical equation for the index
  # of refraction of seawater.’, Appl. Opt., 34, pp. 3477–3480.

  n0 = 1.31405
  n1 = 1.779*1e-4
  n2 = -1.05*1e-6
  n3 = 1.6*1e-8
  n4 = -2.02*1e-6
  n5 = 15.868
  n6 = 0.01155
  n7 = -0.00423
  n8 = -4382
  n9 = 1.1455*1e6

  nw = n0 + (n1+n2*temperature+n3*temperature^2)*salinity+n4*temperature^2+
    (n5+n6*salinity+n7*temperature)/wavelength+n8/wavelength^2+n9/wavelength^3

  return(nw)
}

water_air_radiance_trans <- function(salinity, temperature, wavelength) {
  # Voss, K.J. and Flora, S. (2017) ‘Spectral Dependence of the Seawater–Air
  # Radiance Transmission Coefficient’, Journal of Atmospheric and Oceanic
  # Technology, 34(6), pp. 1203–1205.
  # Available at: https://doi.org/10.1175/JTECH-D-17-0040.1.

  na = 1 # air refractive index

  nw = water_refractive_index(salinity, temperature, wavelength)

  t_radiance = ((4*na*nw)/(na+nw)^2)*(na/nw)^2

  return(t_radiance)
}

compute_rel_unc <- function(combined_mad, reduced_mad) {
  # We compute on variance as it's additive
  # ((base_sd^2 - perturbed_sd^2) / base_sd^2)
  rel_unc <- abs(combined_mad - reduced_mad) / combined_mad
  return(rel_unc)
}
