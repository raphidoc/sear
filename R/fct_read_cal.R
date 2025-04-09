#' hocr_cal
#'
#' @description parse the calibration files of satlantic HOCR
#'
#' @return a list with one tidy data frame per fit type
#'
#' Read HOCR calibration file
#'
#' @author Raphael Mabit
#'
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import readr
#' @importFrom rlang .data
#'
#' @noRd

read_hocr_cal <- function(CalFiles) {
  # CalFiles is a list of calibration files

  # Fit type specific coefficients
  # OPTIC3 fit type: a0, a1, im, cint
  # THERM1 fit type: m0 m1 m2 m3 Tr
  # POLYU fit type: a0 a1

  # Create one tidy data frame per fit type
  CalFile <- readr::read_lines(CalFiles, skip_empty_rows = T)

  CalFile <- CalFile[!stringr::str_detect(CalFile, "#")]

  # Cal file have tow types of delimiter ... space for definition line and tab for calibration coefficient

  # Working code over there !
  CalFile <- stringr::str_replace_all(CalFile, "\\t", " ")

  # Get index of definition file
  indi <- purrr::imap_dbl(CalFile, ~ ifelse(str_detect(.x, "1 (OPTIC3|THERM1|POLYU)"), .y, NA))
  # Calibration data is on the next line
  cal_data <- CalFile[indi + 1]
  # Put definition and calibration on the same line
  CalFile <- tibble(Def = CalFile, Cal = cal_data)
  # Dirty Stuff to remove calibration lines: offset index by one to match calibration lines and logically remove them
  CalFile <- CalFile %>% filter(is.na(!append(indi[-length(indi)], NA, 0)))

  CalFile <- CalFile %>%
    separate(
      col = .data$Def,
      into = c("type", "id", "units", "field_length", "data_type", "cal_lines", "fit_type"),
      sep = " ",
      remove = TRUE,
      convert = TRUE
    )

  CalID <- CalFile %>%
    filter(type %in% c("INSTRUMENT", "SN")) %>%
    select(type, id) %>%
    pivot_wider(
      names_from = type,
      values_from = id
    ) %>%
    rename(instrument = INSTRUMENT, sn = SN)

  OPTIC3 <- CalFile %>%
    filter(.data$type %in% c("ES", "LU", "Lu")) %>%
    separate(
      col = .data$Cal,
      into = c("a0", "a1", "im", "cint"),
      sep = " ",
      remove = TRUE,
      convert = TRUE
    ) %>%
    bind_cols(CalID) %>%
    rename(wavelength = "id") %>%
    mutate(wavelength = as.numeric(.data$wavelength))

  if (nrow(OPTIC3) == 0) {
    stop("OPTIC3 calibration is empty")
  }

  THERM1 <- CalFile %>%
    filter(.data$fit_type == "THERM1") %>%
    separate(
      col = .data$Cal,
      into = c("m0", "m1", "m2", "m3", "Tr"),
      sep = " ",
      remove = TRUE,
      convert = TRUE
    ) %>%
    bind_cols(CalID)

  if (nrow(THERM1) == 0) {
    warning("THERM1 calibration is empty")
  }

  # POLYU span from a0 to an, should write code to take that into account
  # Sep in cal file appear to be two spaces ... cloud manage all those with "[[:blank:]]"
  POLYU <- CalFile %>%
    filter(.data$fit_type == "POLYU") %>%
    separate(
      col = .data$Cal,
      into = c("a0", "a1"),
      sep = "(?<!^)[[:blank:]]{2}|(?<=[0-9])[[:blank:]](?=[0-9])",
      remove = TRUE,
      convert = TRUE
    ) %>%
    bind_cols(CalID)

  if (nrow(POLYU) == 0) {
    stop("POLYU calibration is empty")
  }

  return(list(OPTIC3 = OPTIC3, THERM1 = THERM1, POLYU = POLYU))
}

tidy_cal_hocr <- function(CalFiles) {
  # CalFiles <- list.files(system.file("cal", "hocr", package = "sear"), full.names = TRUE)

  CalList <- purrr::map(CalFiles, read_hocr_cal)

  FlatCal <- unlist(CalList, recursive = FALSE)

  OPTIC3 <- bind_rows(FlatCal[names(FlatCal) == "OPTIC3"])

  THERM1 <- bind_rows(FlatCal[names(FlatCal) == "THERM1"])

  POLYU <- bind_rows(FlatCal[names(FlatCal) == "POLYU"])

  # Normlize type for the app
  OPTIC3 <- OPTIC3 %>%
    mutate(type = case_when(
      type == "Lu" ~ "LU",
      type == "Es" ~ "ES",
      .default = type
    )) %>%
    group_by(.data$instrument, .data$sn) %>%
    nest(OPTIC3 = !matches("instrument|sn"))

  THERM1 <- THERM1 %>%
    group_by(.data$instrument, .data$sn) %>%
    nest(THERM1 = !matches("instrument|sn"))

  INTTIME <- POLYU %>%
    filter(.data$type == "INTTIME") %>%
    group_by(.data$instrument, .data$sn) %>%
    nest(INTTIME = !matches("instrument|sn"))

  SAMPLE <- POLYU %>%
    filter(.data$type == "SAMPLE") %>%
    group_by(.data$instrument, .data$sn) %>%
    nest(SAMPLE = !matches("instrument|sn"))

  list("OPTIC3" = OPTIC3, "THERM1" = THERM1, "INTTIME" = INTTIME, "SAMPLE" = SAMPLE)
}

#' sbe19_cal
#'
#' @description Read SBE19 cal file
#'
#' @return tibble with calibration data
#'
#' @noRd
read_sbe19_cal <- function(CalFile) {
  # CalFile <- sear:::app_sys("cal", "sbe19", "7974.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  cal_data <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "parameter",
        "value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "parameter",
      values_from = "value"
    ) %>%
    mutate(
      t_cal_date = dmy(t_cal_date),
      t_a0 = as.numeric(t_a0),
      t_a1 = as.numeric(t_a1),
      t_a2 = as.numeric(t_a2),
      t_a3 = as.numeric(t_a3),
      c_cal_date = dmy(c_cal_date),
      c_g = as.numeric(c_g),
      c_h = as.numeric(c_h),
      c_i = as.numeric(c_i),
      c_j = as.numeric(c_j),
      c_t_cor = as.numeric(c_t_cor),
      c_p_cor = as.numeric(c_p_cor),
      p_cal_date = dmy(p_cal_date),
      p_a0 = as.numeric(p_a0),
      p_a1 = as.numeric(p_a1),
      p_a2 = as.numeric(p_a2),
      p_t_c_a0 = as.numeric(p_t_c_a0),
      p_t_c_a1 = as.numeric(p_t_c_a1),
      p_t_c_a2 = as.numeric(p_t_c_a2),
      p_t_c_b0 = as.numeric(p_t_c_b0),
      p_t_c_b1 = as.numeric(p_t_c_b1),
      p_t_c_b2 = as.numeric(p_t_c_b2),
      p_temp_a0 = as.numeric(p_temp_a0),
      p_temp_a1 = as.numeric(p_temp_a1),
      p_temp_a2 = as.numeric(p_temp_a2)
    )
}

#' sbe43_cal
#'
#' @description read SBE43 cal file
#'
#' @return tibble with calibration data
#'
#' @noRd
read_sbe43_cal <- function(CalFile) {
  # CalFile <- sear:::app_sys("cal", "sbe43", "3625.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  cal_data <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "parameter",
        "value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "parameter",
      values_from = "value"
    ) %>%
    mutate(
      o_cal_date = dmy(o_cal_date),
      soc = as.numeric(soc),
      voffset = as.numeric(voffset),
      a = as.numeric(a),
      b = as.numeric(b),
      c = as.numeric(c),
      e = as.numeric(e),
      tau_20 = as.numeric(tau_20),
    )
}

#' sbe18_cal
#'
#' @description read SBE18 cal file
#'
#' @return tibble with calibration data
#'
#' @noRd
read_sbe18_cal <- function(CalFile) {
  # CalFile <- sear:::app_sys("cal", "sbe18", "1494.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  cal_data <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "parameter",
        "value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "parameter",
      values_from = "value"
    ) %>%
    mutate(
      ph_cal_date = dmy(ph_cal_date),
      slope = as.numeric(slope),
      offset = as.numeric(offset)
    )
}

#' seaowl_cal
#'
#' @description read SeaOWL cal file
#'
#' @return tibble with calibration data
#'
#' @noRd
read_seaowl_cal <- function(CalFile) {
  # CalFile <- sear:::app_sys("cal", "seaowl", "0144.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  cal_data <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "parameter",
        "value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "parameter",
      values_from = "value"
    ) %>%
    mutate(
      cal_date = dmy(cal_date),
      vsf_scale_factor = as.numeric(vsf_scale_factor),
      vsf_dark_count = as.numeric(vsf_dark_count),
      chl_scale_factor = as.numeric(chl_scale_factor),
      chl_dark_count = as.numeric(chl_dark_count),
      oil_scale_factor = as.numeric(oil_scale_factor),
      oil_dark_count = as.numeric(oil_dark_count),
      fdom_scale_factor = as.numeric(fdom_scale_factor),
      fdom_dark_count = as.numeric(fdom_dark_count)
    )
}

#' seaowl_cal
#'
#' @description read SeaOWL cal file
#'
#' @return tibble with calibration data
#'
#' @noRd
read_bbfl2_cal <- function(CalFile) {
  # CalFile <- sear:::app_sys("cal", "bbfl2", "5745.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  cal_data <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "parameter",
        "value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "parameter",
      values_from = "value"
    ) %>%
    mutate(
      cal_date = dmy(cal_date),
      ntu_scale_factor = as.numeric(ntu_scale_factor),
      ntu_dark_count = as.numeric(ntu_dark_count),
      pe_scale_factor = as.numeric(pe_scale_factor),
      pe_dark_count = as.numeric(pe_dark_count),
      pc_scale_factor = as.numeric(pc_scale_factor),
      pc_dark_count = as.numeric(pc_dark_count)
    )
}
