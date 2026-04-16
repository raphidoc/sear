#' Ir Look Up Table
#' Produced with 6S with midlat-summer gas model, along dimension
#'  {wavelength, theta_sun, target_pressure, aod_550}.
#'  Reduced to dimension wavelength and theta_sun to propagate uncertainty
#'  associated to aod_550 in determination of diffuse to direct irradiance
#'  ration for self shading correction.
#'  Effect of target_pressure is negligible compared to aod variability.
"ir_lut"
