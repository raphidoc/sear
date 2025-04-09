#' unc_monte_carlo
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

propagate_unc_cov <- function(
    wavelength,
    es,
    luz1,
    luz2,
    cov_mat,
    ctd_pdf,
    meta_temp,
    a_temp,
    ir,
    n_draws = 1e5
) {

  set.seed(1234)

  # Make matrices with no uncertainty

  a_samples_fixed <- matrix(rep(a_temp$a_median, n_draws), nrow = n_draws, byrow = T)

  ir_samples_fixed <- matrix(rep(ir, n_draws), nrow = n_draws, byrow = T)

  shading_coeff_fixed <- lu_shading_coeff(
    theta_sun = meta_temp$sol_zen_median,
    salinity = ctd_pdf$salinity_absolute_median,
    temperature = ctd_pdf$temperature_median,
    wavelength = wavelength,
    #h = 0.001, # distance between detector (sensor) and optical window
    #fov = 8, # Half angle field of view
    sensor_radius = 0.003,
    instrument_radius = 0.03, # instrument radius 3 cm for HOCR
    ir = ir_samples_fixed, # diffuse to direct sun irradiance
    a = a_samples_fixed# Total absorption coefficient
  )

  luz1_samples_fixed <- matrix(rep(luz1$channel_median, n_draws), nrow = n_draws, byrow = T)
  luz2_samples_fixed <- matrix(rep(luz2$channel_median, n_draws), nrow = n_draws, byrow = T)

  z1_samples_fixed <- matrix(
    rep(rep(ctd_pdf$z1_median, length(wavelength)), n_draws),
    nrow = n_draws, byrow = F
  )

  temperature_samples_fixed <- matrix(
    rep(rep(ctd_pdf$temperature_median, length(wavelength)), n_draws),
    nrow = n_draws, byrow = F
  )

  salinity_samples_fixed <- matrix(
    rep(rep(ctd_pdf$salinity_absolute_median, length(wavelength)), n_draws),
    nrow = n_draws, byrow = F
  )

  es_samples_fixed <- matrix(rep(es$channel_median, n_draws), nrow = n_draws, byrow = T)

  # Draw matrices with uncertainty

  # a_samples <- purrr::map2(
  #   .x = a_temp$a_median,
  #   .y = a_temp$a_mad,
  #   ~ rnorm(n_draws, .x, .y)
  # )

  a_samples <- purrr::map2(
    .x = a_temp$a_min,
    .y = a_temp$a_max,
    ~ runif(n_draws, .x, .y)
  )
  a_samples <- matrix(unlist(a_samples), nrow = n_draws, byrow = FALSE)

  ir_samples <- matrix(rep(ir, n_draws), nrow = n_draws, byrow = T)

  shading_coeff <- lu_shading_coeff(
    theta_sun = meta_temp$sol_zen_median,
    salinity = ctd_pdf$salinity_absolute_median,
    temperature = ctd_pdf$temperature_median,
    wavelength = wavelength,
    #h = 0.02, # distance between detector (sensor) and optical window (meter ?)
    #fov = 8, # Half angle field of view
    sensor_radius = 0.003,
    instrument_radius = 0.03, # instrument radius 3 cm for HOCR
    ir = ir_samples, # diffuse to direct sun irradiance
    a = a_samples# Total absorption coefficient
  )

  #With correlation
  cov_samples <- purrr::pmap(
    list(
      ..1 = luz1$channel_median,
      ..2 = luz2$channel_median,
      ..3 = ctd_pdf$z1_median,
      ..4 = cov_mat
    ),
    ~ mvrnorm(
      n_draws, c(..1, ..2, ..3), ..4, tol = 1e10
    )
  )

  luz1_samples <- purrr::map(
    .x = cov_samples,
    ~ .x[,1]
  )
  luz1_samples <- matrix(unlist(luz1_samples), nrow = n_draws, byrow = FALSE)

  luz2_samples <- purrr::map(
    .x = cov_samples,
    ~ .x[,2]
  )
  luz2_samples <- matrix(unlist(luz2_samples), nrow = n_draws, byrow = FALSE)

  # ply <- plot_ly() %>%
  #   add_lines(
  #     x = wavelength,
  #     y = colMeans(luz2_samples)
  #   ) %>% add_ribbons(
  #     x = wavelength,
  #     ymin = colMeans(luz2_samples) - apply(luz2_samples, 2, sd),
  #     ymax = colMeans(luz2_samples) + apply(luz2_samples, 2, sd)
  #   )

  z1_samples <- purrr::map(
    .x = cov_samples,
    ~ .x[,3]
  )
  z1_samples <- matrix(unlist(z1_samples), nrow = n_draws, byrow = FALSE)

  # z2z1 uncertainty come from the tilt of the platform
  # with 10 degree of tilt z2z1 is reduced to 0.14772 m
  z2z1_samples <- matrix(
    rep(
      rnorm(n_draws, meta_temp$tilt_median, meta_temp$tilt_mad),
      length(wavelength)
    ),
    nrow = n_draws, byrow = F)
  z2z1_samples <- cos(z2z1_samples*pi/180) * 0.15

  salinity_samples <- matrix(
    rep(
      rnorm(n_draws, ctd_pdf$salinity_absolute_median, ctd_pdf$salinity_absolute_sd),
      length(wavelength)
    ),
    nrow = n_draws, byrow = F)

  temperature_samples <- matrix(
    rep(
      rnorm(n_draws, ctd_pdf$temperature_median, ctd_pdf$temperature_sd),
      length(wavelength)
    ),
    nrow = n_draws, byrow = F)

  es_samples <- purrr::map2(
    .x = es$channel_median,
    .y = es$channel_sd,
    ~ rnorm(
      n_draws, .x, .y
    )
  )
  es_samples <- matrix(unlist(es_samples), nrow = n_draws, byrow = FALSE)

  # Compute Rrs unshaded
  klu_compound_unshaded <- compute_klu(
    wavelength,
    luz1_samples,
    luz2_samples,
    z2z1 = 0.15
  )
  klu_estimates_compound_unshaded <- klu_compound_unshaded[[1]]
  klu_samples_compound_unshaded <- klu_compound_unshaded[[2]]

  lw_compound_unshaded <- compute_lw(
    wavelength,
    luz1_samples,
    klu_samples_compound_unshaded,
    z1_samples,
    salinity_samples,
    temperature_samples
  )

  lw_estimates_compound_unshaded <- lw_compound_unshaded[[1]] %>%
    left_join(klu_estimates_compound_unshaded, by = "wavelength")
  lw_samples_compound_unshaded <- lw_compound_unshaded[[2]]

  rrs_compound_unshaded <- compute_rrs(
    wavelength,
    lw_samples_compound_unshaded,
    es_samples
  ) %>%
    left_join(lw_estimates_compound_unshaded, by = "wavelength")

  # Compute Rrs with full uncertainties
  luz1_samples_shade <- luz1_samples * shading_coeff
  luz2_samples_shade <- luz2_samples * shading_coeff

  klu_compound <- compute_klu(
    wavelength,
    luz1_samples_shade,
    luz2_samples_shade,
    z2z1 = 0.15
  )
  klu_estimates_compound <- klu_compound[[1]]
  klu_samples_compound <- klu_compound[[2]]

  lw_compound <- compute_lw(
    wavelength,
    luz1_samples_shade,
    klu_samples_compound,
    z1_samples,
    salinity_samples,
    temperature_samples
  )

  lw_estimates_compound <- lw_compound[[1]] %>%
    left_join(klu_estimates_compound, by = "wavelength")
  lw_samples_compound <- lw_compound[[2]]

  rrs_compound <- compute_rrs(
    wavelength,
    lw_samples_compound,
    es_samples
  ) %>%
    left_join(lw_estimates_compound, by = "wavelength")

  # Compute Rrs with no uncertainties
  luz1_samples_shade_fixed <- luz1_samples_fixed * shading_coeff_fixed
  luz2_samples_shade_fixed <- luz2_samples_fixed * shading_coeff_fixed

  klu_fixed <- compute_klu(
    wavelength,
    luz1_samples_shade_fixed,
    luz2_samples_shade_fixed,
    z2z1 = 0.15
  )
  klu_estimates_fixed <- klu_fixed[[1]]
  klu_samples_fixed <- klu_fixed[[2]]

  lw_fixed <- compute_lw(
    wavelength,
    luz1_samples_shade_fixed,
    klu_samples_fixed,
    z1_samples_fixed,
    salinity_samples_fixed,
    temperature_samples_fixed
  )

  lw_estimates_fixed <- lw_fixed[[1]] %>%
    left_join(klu_estimates_fixed, by = "wavelength")
  lw_samples_fixed <- lw_fixed[[2]]

  rrs_fixed <- compute_rrs(
    wavelength,
    lw_samples_fixed,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_fixed, by = "wavelength")

  # Compute shading uncertainties
  luz1_rel_samples <- luz1_samples_fixed * shading_coeff
  luz2_rel_samples <- luz2_samples_fixed * shading_coeff

  klu_rel_shade <- compute_klu(
    wavelength,
    luz1_rel_samples,
    luz2_rel_samples,
    z2z1 = 0.15
  )
  klu_estimates_rel_shade <- klu_rel_shade[[1]]
  klu_samples_rel_shade <- klu_rel_shade[[2]]

  lw_rel_shade <- compute_lw(
    wavelength,
    luz1_rel_samples,
    klu_samples_rel_shade,
    z1_samples_fixed,
    salinity_samples_fixed,
    temperature_samples_fixed
  )

  lw_estimates_rel_shade <- lw_rel_shade[[1]] %>%
    left_join(klu_estimates_rel_shade, by = "wavelength")
  lw_samples_rel_shade <- lw_rel_shade[[2]]

  rrs_rel_shade <- compute_rrs(
    wavelength,
    lw_samples_rel_shade,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_rel_shade, by = "wavelength")

  # luz1 uncertainty
  luz1_rel_samples <- luz1_samples * shading_coeff_fixed

  klu_rel_luz1 <- compute_klu(
    wavelength,
    luz1_rel_samples,
    luz2_samples_shade_fixed,
    z2z1 = 0.15
  )
  klu_estimates_rel_luz1 <- klu_rel_luz1[[1]]
  klu_samples_rel_luz1 <- klu_rel_luz1[[2]]

  lw_rel_luz1 <- compute_lw(
    wavelength,
    luz1_rel_samples,
    klu_samples_rel_luz1,
    z1_samples_fixed,
    salinity_samples_fixed,
    temperature_samples_fixed
  )

  lw_estimates_rel_luz1 <- lw_rel_luz1[[1]] %>%
    left_join(klu_estimates_rel_luz1, by = "wavelength")
  lw_samples_rel_luz1 <- lw_rel_luz1[[2]]

  rrs_rel_luz1 <- compute_rrs(
    wavelength,
    lw_samples_rel_luz1,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_rel_luz1, by = "wavelength")

  # luz2 uncertainty
  luz2_rel_samples <- luz2_samples * shading_coeff_fixed

  klu_rel_luz2 <- compute_klu(
    wavelength,
    luz1_samples_shade_fixed,
    luz2_rel_samples,
    z2z1 = 0.15
  )
  klu_estimates_rel_luz2 <- klu_rel_luz2[[1]]
  klu_samples_rel_luz2 <- klu_rel_luz2[[2]]

  lw_rel_luz2 <- compute_lw(
    wavelength,
    luz1_samples_shade_fixed,
    klu_samples_rel_luz2,
    z1_samples_fixed,
    salinity_samples_fixed,
    temperature_samples_fixed
  )

  lw_estimates_rel_luz2 <- lw_rel_luz2[[1]] %>%
    left_join(klu_estimates_rel_luz2, by = "wavelength")
  lw_samples_rel_luz2 <- lw_rel_luz2[[2]]

  rrs_rel_luz2 <- compute_rrs(
    wavelength,
    lw_samples_rel_luz2,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_rel_luz2, by = "wavelength")

  # z1z2 uncertainty
  klu_rel_z1z2 <- compute_klu(
    wavelength,
    luz1_samples_shade_fixed,
    luz2_samples_shade_fixed,
    z2z1 = z2z1_samples
  )
  klu_estimates_rel_z1z2 <- klu_rel_z1z2[[1]]
  klu_samples_rel_z1z2 <- klu_rel_z1z2[[2]]

  lw_rel_z1z2 <- compute_lw(
    wavelength,
    luz1_samples_shade_fixed,
    klu_samples_rel_z1z2,
    z1_samples_fixed,
    salinity_samples_fixed,
    temperature_samples_fixed
  )

  lw_estimates_rel_z1z2 <- lw_rel_z1z2[[1]] %>%
    left_join(klu_estimates_rel_z1z2, by = "wavelength")
  lw_samples_rel_z1z2 <- lw_rel_z1z2[[2]]

  rrs_rel_z1z2 <- compute_rrs(
    wavelength,
    lw_samples_rel_z1z2,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_rel_z1z2, by = "wavelength")

  # z1 uncertainty
  lw_rel_z1 <- compute_lw(
    wavelength,
    luz1_samples_shade_fixed,
    klu_samples_fixed,
    z1_samples,
    salinity_samples_fixed,
    temperature_samples_fixed
  )

  lw_estimates_rel_z1 <- lw_rel_z1[[1]]
  lw_samples_rel_z1 <- lw_rel_z1[[2]]

  rrs_rel_z1 <- compute_rrs(
    wavelength,
    lw_samples_rel_z1,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_rel_z1, by = "wavelength")

  # temperature uncertainty
  lw_rel_temperature <- compute_lw(
    wavelength,
    luz1_samples_shade_fixed,
    klu_samples_fixed,
    z1_samples_fixed,
    salinity_samples_fixed,
    temperature_samples
  )

  lw_estimates_rel_temperature <- lw_rel_temperature[[1]]
  lw_samples_rel_temperature <- lw_rel_temperature[[2]]

  rrs_rel_temperature <- compute_rrs(
    wavelength,
    lw_samples_rel_temperature,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_rel_temperature, by = "wavelength")

  # salinity uncertainty
  lw_rel_salinity <- compute_lw(
    wavelength,
    luz1_samples_shade_fixed,
    klu_samples_fixed,
    z1_samples_fixed,
    salinity_samples,
    temperature_samples_fixed
  )

  lw_estimates_rel_salinity <- lw_rel_salinity[[1]]
  lw_samples_rel_salinity <- lw_rel_salinity[[2]]

  rrs_rel_salinity <- compute_rrs(
    wavelength,
    lw_samples_rel_salinity,
    es_samples_fixed
  ) %>%
    left_join(lw_estimates_rel_salinity, by = "wavelength")

  # es uncertainty
  rrs_rel_es <- compute_rrs(
    wavelength,
    lw_samples_fixed,
    es_samples
  )

  rrs <-tibble(
    wavelength = wavelength,
    shade_estimate = colMeans(shading_coeff),
    shade_max = apply(shading_coeff, 2, max),
    shade_min = apply(shading_coeff, 2, min),
    klu_unc =
      rrs_rel_shade$klu_unc +
      rrs_rel_luz1$klu_unc +
      rrs_rel_luz2$klu_unc +
      rrs_rel_z1z2$klu_unc,
    klu_shade_unc = rrs_rel_shade$klu_unc / klu_unc,
    klu_luz1_unc = rrs_rel_luz1$klu_unc / klu_unc,
    klu_luz2_unc = rrs_rel_luz2$klu_unc / klu_unc,
    klu_z2z1_unc = rrs_rel_z1z2$klu_unc / klu_unc,
    klu_estimate = rrs_compound$klu_estimate,
    klu_estimate_compound = rrs_compound$klu_estimate,
    klu_unc_compound = rrs_compound$klu_unc,
    lw_unc =
      rrs_rel_shade$lw_unc +
      rrs_rel_luz1$lw_unc +
      rrs_rel_luz2$lw_unc +
      rrs_rel_z1z2$lw_unc +
      rrs_rel_z1$lw_unc +
      rrs_rel_temperature$lw_unc +
      rrs_rel_salinity$lw_unc ,
    lw_shade_unc = rrs_rel_shade$lw_unc / lw_unc,
    lw_luz1_unc = rrs_rel_luz1$lw_unc / lw_unc,
    lw_luz2_unc = rrs_rel_luz2$lw_unc / lw_unc,
    lw_z2z1_unc = rrs_rel_z1z2$lw_unc / lw_unc,
    lw_z1_unc = rrs_rel_z1$lw_unc / lw_unc,
    lw_temperature_unc = rrs_rel_temperature$lw_unc / lw_unc,
    lw_salinity_unc = rrs_rel_salinity$lw_unc / lw_unc,
    lw_estimate = rrs_fixed$lw_estimate,
    lw_estimate_compound = rrs_compound$lw_estimate,
    lw_unc_compound =  rrs_compound$lw_unc,
    rrs_unc =
      rrs_rel_shade$rrs_unc +
      rrs_rel_luz1$rrs_unc +
      rrs_rel_luz2$rrs_unc +
      rrs_rel_z1z2$rrs_unc +
      rrs_rel_z1$rrs_unc +
      rrs_rel_temperature$rrs_unc +
      rrs_rel_salinity$rrs_unc +
      rrs_rel_es$rrs_unc,
    rrs_shade_unc = rrs_rel_shade$rrs_unc / rrs_unc,
    rrs_luz1_unc = rrs_rel_luz1$rrs_unc / rrs_unc,
    rrs_luz2_unc = rrs_rel_luz2$rrs_unc / rrs_unc,
    rrs_z2z1_unc = rrs_rel_z1z2$rrs_unc / rrs_unc,
    rrs_z1_unc = rrs_rel_z1$rrs_unc / rrs_unc,
    rrs_temperature_unc = rrs_rel_temperature$rrs_unc / rrs_unc,
    rrs_salinity_unc = rrs_rel_salinity$rrs_unc / rrs_unc,
    rrs_es_unc = rrs_rel_es$rrs_unc / rrs_unc,
    rrs_estimate = rrs_fixed$rrs_estimate,
    rrs_estimate_compound = rrs_compound$rrs_estimate,
    rrs_unc_compound = rrs_compound$rrs_unc,
    rrs_estimate_compound_unshaded = rrs_compound_unshaded$rrs_estimate,
    rrs_unc_compound_unshaded = rrs_compound_unshaded$rrs_unc
  )

  return(rrs)
}

# propagate_unc_cov <- function(
    #   wavelength,
#   es,
#   luz1,
#   luz2,
#   cov_mat,
#   ctd,
#   n_draws
# ) {
#
#   set.seed(1234)
#
#   # browser()
#
#   # make.positive.definite(nn, tol=1e-3)
#
#   #With correlation
#   cov_samples <- purrr::pmap(
#     list(
#       ..1 = luz1$channel_median,
#       ..2 = luz2$channel_median,
#       ..3 = ctd$z1_median,
#       ..4 = cov_mat
#     ),
#     ~ MASS::mvrnorm(
#       n_draws, c(..1, ..2, ..3), ..4, tol = 1e10
#     )
#   )
#
#   luz1_samples <- purrr::map(
#     .x = cov_samples,
#     ~ .x[,1]
#   )
#   luz1_samples <- matrix(unlist(luz1_samples), nrow = n_draws, byrow = FALSE)
#
#   luz2_samples <- purrr::map(
#     .x = cov_samples,
#     ~ .x[,2]
#   )
#   luz2_samples <- matrix(unlist(luz2_samples), nrow = n_draws, byrow = FALSE)
#
#   z1_samples <- purrr::map(
#     .x = cov_samples,
#     ~ .x[,3]
#   )
#   z1_samples <- matrix(unlist(z1_samples), nrow = n_draws, byrow = FALSE)
#
#   # browser()
#   #
#   # ply <- plot_ly() %>%
#   # add_histogram(
#   #   x = luz1_samples[,50]
#   # )
#   #
#   # save_image(ply, here("1_chapter/figure/diag/luz1503_samples_hist.png"), width = 700, height = 500, scale = 4)
#   #
#   klu <- compute_klu(wavelength, luz1_samples, luz2_samples)
#   klu_estimates <- klu[[1]]
#   klu_samples <- klu[[2]]
#
#   salinity_samples <- matrix(
#     rep(
#       rnorm(n_draws, ctd$salinity_absolute_median, ctd$salinity_absolute_sd),
#       length(wavelength)
#     ),
#     nrow = n_draws, byrow = F)
#
#   temperature_samples <- matrix(
#     rep(
#       rnorm(n_draws, ctd$temperature_median, ctd$temperature_sd),
#       length(wavelength)
#     ),
#     nrow = n_draws, byrow = F)
#
#   lw <- compute_lw(
#     wavelength,
#     luz1_samples,
#     klu_samples,
#     z1_samples,
#     salinity_samples,
#     temperature_samples
#   )
#
#   lw_estimates <- lw[[1]] %>%
#     left_join(klu_estimates, by = "wavelength")
#   lw_samples <- lw[[2]]
#
#   es_samples <- purrr::map2(
#     .x = es$channel_median,
#     .y = es$channel_sd,
#     ~ rnorm(
#       n_draws, .x, .y
#     )
#   )
#   es_samples <- matrix(unlist(es_samples), nrow = n_draws, byrow = FALSE)
#
#   message("computing rrs")
#
#   rrs <- compute_rrs(
#     wavelength,
#     lw_samples,
#     es_samples
#   ) %>%
#     left_join(lw_estimates, by = "wavelength")
#
#   # Uncertainty breakdown by input
#   # Lu(z1) uncertainty
#   message("Lu(z1) uncertainty")
#
#   luz1_rel_samples <- matrix(rep(luz1$channel_median, n_draws), nrow = n_draws, byrow = T)
#
#   klu_luz1_rel <- compute_klu(
#     wavelength,
#     luz1_rel_samples,
#     luz2_samples
#   )
#
#   klu_luz1_rel_estimates <- klu_luz1_rel[[1]]
#   klu_luz1_rel_samples <- klu_luz1_rel[[2]]
#
#   lw_luz1_rel <- compute_lw(
#     wavelength,
#     luz1_rel_samples,
#     klu_luz1_rel_samples,
#     z1_samples,
#     salinity_samples,
#     temperature_samples
#   )
#
#   lw_luz1_rel_estimates <- lw_luz1_rel[[1]]
#   lw_luz1_rel_samples <- lw_luz1_rel[[2]]
#
#   rrs_luz1_rel <- compute_rrs(
#     wavelength,
#     lw_luz1_rel_samples,
#     es_samples
#   )
#
#   # Lu(z2) uncertainty
#   message("Lu(z2) uncertainty")
#
#   luz2_rel_samples <- matrix(rep(luz2$channel_median, n_draws), nrow = n_draws, byrow = T)
#
#   klu_luz2_rel <- compute_klu(
#     wavelength,
#     luz1_samples,
#     luz2_rel_samples
#   )
#
#   klu_luz2_rel_estimates <- klu_luz2_rel[[1]]
#   klu_luz2_rel_samples <- klu_luz2_rel[[2]]
#
#   lw_luz2_rel <- compute_lw(
#     wavelength,
#     luz1_samples,
#     klu_luz2_rel_samples,
#     z1_samples,
#     salinity_samples,
#     temperature_samples
#   )
#
#   lw_luz2_rel_estimates <- lw_luz2_rel[[1]]
#   lw_luz2_rel_samples <- lw_luz2_rel[[2]]
#
#   rrs_luz2_rel <- compute_rrs(
#     wavelength,
#     lw_luz2_rel_samples,
#     es_samples
#   )
#
#   # Z1 uncertainty
#   message("z1 uncertainty")
#
#   z1_rel_samples <- matrix(
#     rep(rep(ctd$z1_median, length(wavelength)), n_draws),
#     nrow = n_draws, byrow = F)
#
#   lw_z1_rel <- compute_lw(
#     wavelength,
#     luz1_samples,
#     klu_samples,
#     z1_rel_samples,
#     salinity_samples,
#     temperature_samples
#   )
#
#   lw_z1_rel_estimates <- lw_z1_rel[[1]]
#   lw_z1_rel_samples <- lw_z1_rel[[2]]
#
#   rrs_z1_rel <- compute_rrs(
#     wavelength,
#     lw_z1_rel_samples,
#     es_samples
#   )
#
#   # temperature uncertainty
#
#   message("temperature uncertainty")
#
#   temp_rel_samples <- matrix(
#     rep(rep(ctd$temperature_median, length(wavelength)), n_draws),
#     nrow = n_draws, byrow = F)
#
#   lw_temp_rel <- compute_lw(
#     wavelength,
#     luz1_samples,
#     klu_samples,
#     z1_samples,
#     salinity_samples,
#     temp_rel_samples
#   )
#
#   lw_temp_rel_estimates <- lw_temp_rel[[1]]
#   lw_temp_rel_samples <- lw_temp_rel[[2]]
#
#   rrs_temp_rel <- compute_rrs(
#     wavelength,
#     lw_temp_rel_samples,
#     es_samples
#   )
#
#   # salinity uncertainty
#
#   message("salinity uncertainty")
#
#   sal_rel_samples <- matrix(
#     rep(rep(ctd$salinity_absolute_median, length(wavelength)), n_draws),
#     nrow = n_draws, byrow = F)
#
#   lw_sal_rel <- compute_lw(
#     wavelength,
#     luz1_samples,
#     klu_samples,
#     z1_samples,
#     sal_rel_samples,
#     temperature_samples
#   )
#
#   lw_sal_rel_estimates <- lw_sal_rel[[1]]
#   lw_sal_rel_samples <- lw_sal_rel[[2]]
#
#   rrs_sal_rel <- compute_rrs(
#     wavelength,
#     lw_sal_rel_samples,
#     es_samples
#   )
#
#   # Es uncertainty
#   message("Es uncertainty")
#
#   es_rel_samples <- matrix(rep(es$channel_median, n_draws), nrow = n_draws, byrow = T)
#
#   rrs_es_rel <- compute_rrs(
#     wavelength,
#     lw_samples,
#     es_rel_samples
#   )
#
#   # Compute relative uncertainty for each input and check for unity
#
#   rrs <- rrs %>%
#     mutate(
#       klu_luz1_rel_unc = compute_rel_unc(klu_mad, klu_luz1_rel_estimates$klu_mad),
#       klu_luz2_rel_unc = compute_rel_unc(klu_mad, klu_luz2_rel_estimates$klu_mad),
#       klu_rel_unity = klu_luz1_rel_unc + klu_luz2_rel_unc,
#       lw_luz1_rel_unc = compute_rel_unc(lw_mad, lw_luz1_rel_estimates$lw_mad),
#       lw_luz2_rel_unc = compute_rel_unc(lw_mad, lw_luz2_rel_estimates$lw_mad),
#       lw_z1_rel_unc = compute_rel_unc(lw_mad, lw_z1_rel_estimates$lw_mad),
#       lw_temp_rel_unc = compute_rel_unc(lw_mad, lw_temp_rel_estimates$lw_mad),
#       lw_sal_rel_unc = compute_rel_unc(lw_mad, lw_sal_rel_estimates$lw_mad),
#       lw_rel_unity = lw_luz1_rel_unc + lw_luz2_rel_unc + lw_z1_rel_unc + lw_temp_rel_unc + lw_sal_rel_unc,
#       rrs_luz1_rel_unc = compute_rel_unc(rrs_mad, rrs_luz1_rel$rrs_mad),
#       rrs_luz2_rel_unc = compute_rel_unc(rrs_mad, rrs_luz2_rel$rrs_mad),
#       rrs_z1_rel_unc = compute_rel_unc(rrs_mad, rrs_z1_rel$rrs_mad),
#       rrs_temp_rel_unc = compute_rel_unc(rrs_mad, rrs_temp_rel$rrs_mad),
#       rrs_sal_rel_unc = compute_rel_unc(rrs_mad, rrs_sal_rel$rrs_mad),
#       rrs_es_rel_unc = compute_rel_unc(rrs_mad, rrs_es_rel$rrs_mad),
#       rrs_rel_unity = rrs_luz1_rel_unc + rrs_luz2_rel_unc + rrs_z1_rel_unc + rrs_es_rel_unc
#     )
#
#   return(rrs)
# }


#' compute_cov
#' compute covariance between correlated parameter
#' for sampling along covariant distribution

compute_cov <- function(df, ctd) {

  es <- df  %>%
    filter(sn %in% c("1396", "1397"))

  luz1 <- df %>%
    filter(sn %in% c("1413", "1415"))

  luz2 <- df %>%
    filter(sn %in% c("1414", "1416"))

  wavelength <- unique(df$wavelength)

  time_out <- unique(luz2$date_time)

  luz1 <- luz1 %>%
    group_by(wavelength) %>%
    nest() %>%
    mutate(
      approx = purrr::map(
        .x = data,
        ~ tibble(
          date_time = time_out,
          luz1 = approx(.x$date_time, .x$channel, time_out)$y
        )
      )
    ) %>%
    unnest(cols = c(approx)) %>%
    select(!data) %>%
    rename(wavelength = wavelength)

  luz2 <- tibble(
    date_time = luz2$date_time,
    wavelength = luz2$wavelength,
    luz2 = luz2$channel
  )

  cor_df <- left_join(luz2, luz1, by = c("date_time", "wavelength"))

  if (is.null(ctd$date_time)) {
    z1 <- tibble(
      date_time = time_out,
      z1 = ctd$z1
    )

  } else {
    z1 <- tibble(
      date_time = time_out,
      z1 = abs(approx(as.numeric(ymd_hms(ctd$date_time))+2, ctd$z1, time_out)$y)
    )
  }

  cor_df <- left_join(cor_df, z1, by = c("date_time"))

  cov_mat <- cor_df %>%
    select(wavelength, luz1, luz2, z1) %>%
    na.omit()

  cov_mat <- cov_mat %>%
    group_by(wavelength) %>%
    nest() %>%
    mutate(
      cov_mat = purrr::map(
        .x = data,
        ~ MASS::cov.rob(.x, method = "classical")$cov
      )
    )

  return(cov_mat$cov_mat)
}
