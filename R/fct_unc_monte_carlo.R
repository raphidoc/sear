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
    ir_temp,
    n_draws = 1e5
) {

  set.seed(1234)

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

  # ir_samples <- matrix(rep(ir, n_draws), nrow = n_draws, byrow = T)
  ir_samples <- purrr::map2(
    .x = ir_temp$ir_min,
    .y = ir_temp$ir_max,
    ~ runif(n_draws, min = .x,  max = .y)
  )
  ir_samples <- matrix(unlist(ir_samples), nrow = n_draws, byrow = FALSE)

  shading_coeff_total <- lu_shading_coeff(
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
    ~ MASS::mvrnorm(
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

  z1_samples <- purrr::map(
    .x = cov_samples,
    ~ .x[,3]
  )
  z1_samples <- matrix(unlist(z1_samples), nrow = n_draws, byrow = FALSE)

  es_samples <- purrr::map2(
    .x = es$channel_median,
    .y = es$channel_mad,
    ~ rnorm(
      n_draws, .x, .y
    )
  )
  es_samples <- matrix(unlist(es_samples), nrow = n_draws, byrow = FALSE)

  # Compute Rrs with full uncertainties
  luz1_samples_shade <- luz1_samples * shading_coeff_total
  luz2_samples_shade <- luz2_samples * shading_coeff_total

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
    ctd_pdf$salinity_absolute_median,
    ctd_pdf$temperature_median
  )

  lw_estimates_compound <- lw_compound[[1]] %>%
    left_join(klu_estimates_compound, by = "wavelength")
  lw_samples_compound <- lw_compound[[2]]

  rrs_compound <- compute_rrs(
    wavelength,
    lw_samples_compound,
    es_samples
  )

  rrs_estimate_compound <- rrs_compound[[1]] %>%
    left_join(lw_estimates_compound, by = "wavelength")

  rrs_samples_compound <- rrs_compound[[2]]

  # joint MC matrices, dim = c(n_draws, n_wavelength)
  Y_klu <- klu_samples_compound
  Y_lw <- lw_samples_compound
  Y_rrs <- rrs_samples_compound
  X_shade <- shading_coeff_total
  X_luz1 <- luz1_samples
  X_luz2 <- luz2_samples
  X_z1 <- z1_samples
  X_es <- es_samples

  rrs <- tibble(
    wavelength         = wavelength,
    klu_estimate       = rrs_estimate_compound$klu_estimate,
    klu_unc            = rrs_estimate_compound$klu_unc, #apply(Y_klu, 2, mad),
    lw_estimate       = rrs_estimate_compound$lw_estimate,
    lw_unc            = rrs_estimate_compound$lw_unc, #apply(Y_lw, 2, mad),
    rrs_estimate       = rrs_estimate_compound$rrs_estimate,
    rrs_unc            = rrs_estimate_compound$rrs_unc #apply(Y_rrs, 2, mad),
  ) %>%
    bind_cols(
      eta_pair(Y_klu, X_luz1,      "klu_luz1"),
      eta_pair(Y_klu, X_luz2,      "klu_luz2"),
      eta_pair(Y_lw, X_shade, "lw_shade"),
      eta_pair(Y_lw, X_luz1,      "lw_luz1"),
      eta_pair(Y_lw, X_luz2,      "lw_luz2"),
      eta_pair(Y_lw, X_z1,        "lw_z1"),
      eta_pair(Y_rrs, X_shade, "rrs_shade"),
      eta_pair(Y_rrs, X_luz1,      "rrs_luz1"),
      eta_pair(Y_rrs, X_luz2,      "rrs_luz2"),
      eta_pair(Y_rrs, X_z1,        "rrs_z1"),
      eta_pair(Y_rrs, X_es,        "rrs_es")
    ) %>%
    mutate(
      klu_eta2_sum = rowSums(select(., starts_with("klu_") & ends_with("_eta2"))),
      lw_eta2_sum = rowSums(select(., starts_with("lw_") & ends_with("_eta2"))),
      rrs_eta2_sum = rowSums(select(., starts_with("rrs_") & ends_with("_eta2")))
    )

  return(rrs)
}

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

compute_eta2 <- function(y_sd, x_sd, cov_xy, eps = 1e-16) {
  (cov_xy^2) / ((x_sd^2) * (y_sd^2))
  # pmax(0, pmin(1, (cov_xy^2) / pmax(eps, (x_sd^2) * (y_sd^2))))
}

compute_abs_unc <- function(y_sd, eta2) {
  y_sd * sqrt(eta2)
}

eta_pair <- function(y_sample, x_sample, prefix) {
  y_sd   <- apply(y_sample, 2, sd)
  x_sd   <- apply(x_sample, 2, sd)

  cov_xy <- colMeans(y_sample * x_sample, na.rm = T) -
    colMeans(y_sample, na.rm = T) * colMeans(x_sample, na.rm = T)

  eta2 <- compute_eta2(y_sd, x_sd, cov_xy)
  tibble(
    !!paste0(prefix, "_eta2") := eta2,
    !!paste0(prefix, "_unc")  := compute_abs_unc(y_sd, eta2)
  )
}
