#' lu_shading_coeff
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

lu_shading_coeff <- function(
    theta_sun,
    salinity,
    temperature,
    wavelength,
    #h, # distance between detector (sensor) and optical window
    #fov, # Half angle field of view
    sensor_radius = 0.003,
    instrument_radius = 0.03, # instrument radius 3 cm for HOCR
    ir, # diffuse to direct sun irradiance
    a # Total absorption coefficient
) {
  if (!is.matrix(a)) {
    a <- matrix(a, nrow = 1)
  }

  # spectral refraction index
  n_w = water_refractive_index(
    salinity,
    temperature,
    wavelength
  )

  theta_sun = theta_sun * pi / 180
  theta_sun_w = asin(sin(theta_sun)/n_w)

  #sensor_radius = 2 * h * tan(fov * pi / 180) # radiance sensor radius (zibordi 2019)
  # sensor_radius = 0.006
  fr = sensor_radius/instrument_radius

  kpsun = (2.07 + 0.00056 * theta_sun) / tan(theta_sun_w)
  kesun = (1.59 + 0.00063 * theta_sun) / tan(theta_sun_w)
  ksun = (1 - fr) * kpsun + fr * kesun
  ksky = 4.61 - 0.87 * fr

  # epsilon_sun = 1 - exp(ksun * a * instrument_radius)
  # Use sweep to multiply matrix by vector, row-wise
  epsilon_sun = 1 - exp(sweep(a, MARGIN=2, ksun, `*`) * instrument_radius)

  # epsilon_sky = 1 - exp(ksky * a * instrument_radius)
  epsilon_sky = 1 - exp(sweep(a, MARGIN=2, ksky, `*`) * instrument_radius)

  epsilon_lu = (epsilon_sun + epsilon_sky * ir) / (1 + ir)

  return(1 - epsilon_lu)
}
