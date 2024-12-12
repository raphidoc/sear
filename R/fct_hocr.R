#' read_mte_hocr
#'
#' @description Reader for HOCR binary file logged by MTE datalogger.
#' Leverage \href{https://kaitai.io/}{kaitaistruct} with python, the python custom class is lost between
#' session so the object return a null pointer. This make it impossible (?) to save
#' to file so a quick fix was to extract the values and put them in an r list.
#' Check for packet integrity by detecting valid instruments name, this may leave
#' some corrupted packets.
#'
#' @return Return a r list containing all HOCR packet value as character or numeric
#'
#' @noRd
read_mte_hocr <- function(BinFile) {
  # now source python onload
  reticulate::source_python(system.file("py", "hocr_mte.py", package = "sear", mustWork = T))

  message(paste("Reading MTE", basename(BinFile)))

  hocr_raw <- purrr::map(BinFile, HocrMte$from_file)

  hocr_raw <- unlist(purrr::map(hocr_raw, ~ .x$packets))

  # Unefficent way of removing custom class python object dependencies resulting in null pointer
  # How to avoid: Error in py_ref_to_r(x) : Embedded NUL in string ? (packet 27281 of AlgaeValidation (2022/07/05))

  hocr_raw <- purrr::imap(hocr_raw, ~ {
    tryCatch(
      {
        i <- .y
        list(
          "channel" = .$channel,
          "checksum" = .$checksum,
          "darkaverage" = .$darkaverage,
          "darksample" = .$darksample,
          "frame" = .$frame,
          "time" = .$time,
          "instrument" = tryCatch(
            as.character(.$instrument),
            error = function(e) {
              message(paste("Packet", i, "instrument", e))
              return(NA)
            }
          ),
          "inttime" = .$inttime,
          "loggerport" = .$loggerport,
          "mysterydate" = .$mysterydate,
          "sampledelay" = .$sampledelay,
          "sn" = tryCatch(
            as.character(.$sn),
            error = function(e) {
              message(paste("Packet", i, "sn", e))
              return(NA)
            }
          ),
          "spectemp" = tryCatch(
            as.character(.$spectemp),
            error = function(e) {
              message(paste("Packet", i, "spectemp", e))
              return(NA)
            }
          ),
          "timer" = tryCatch(
            as.character(.$timer),
            error = function(e) {
              message(paste("Packet", i, "timer", e))
              return(NA)
            }
          )
        )
      },
      error = function(e) {
        message(paste("Packet", i, e))
        return(NA)
      }
    )
  })

  if (any(is.na(hocr_raw))) {
    na_packet <- which(is.na(hocr_raw))
    hocr_raw <- hocr_raw[-which(is.na(hocr_raw))]
  } else {
    na_packet <- 0
  }

  # check for invalid packet
  ValidInd <- purrr::map_lgl(
    hocr_raw,
    ~ ifelse(
      is.na(.x$instrument),
      F,
      str_detect(.x$instrument, "SAT(HPE|PED|HSE|HED|HPL|PLD|HSL|HLD)")
    )
  )

  if (any(!ValidInd)) {
    message("Invalid HOCR packets detected and removed: ", length(which(!ValidInd)) + na_packet)

    hocr_raw <- hocr_raw[ValidInd]
  }

  return(hocr_raw)
}

#' read_satview_hocr
#'
#' @description Reader for HOCR binary file logged by SatView.
#' Leverage \href{https://kaitai.io/}{kaitaistruct} with python, the python custom class is lost between
#' session so the object return a null pointer. This make it impossible (?) to save
#' to file so a quick fix was to extract the values and put them in an r list.
#' Check for packet integrity by detecting valid instruments name, this may leave
#' some corrupted packets.
#'
#' @return Return a r list containing all HOCR packet value as character or numeric
#'
#' @noRd
read_satview_hocr <- function(RawFile) {
  reticulate::source_python(system.file("py", "hocr_satview.py", package = "sear", mustWork = T))

  hocr_raw <- purrr::map(RawFile, HocrSatview$from_file)

  hocr_raw <- unlist(purrr::map(hocr_raw, ~ .x$packets))

  # Unefficent way of removing custom class python object dependencies resulting in null pointer
  # How to avoid: Error in py_ref_to_r(x) : Embedded NUL in string ? (packet 27281 of AlgaeValidation (2022/07/05))

  hocr_raw <- purrr::imap(.x = hocr_raw, ~ {
    tryCatch(
      {
        i <- .y
        list(
          "channel" = .x$channel,
          "checksum" = .x$checksum,
          "darkaverage" = .x$darkaverage,
          "darksample" = .x$darksample,
          "frame" = .x$frame,
          "time" = .x$time,
          "instrument" = tryCatch(
            as.character(.x$instrument),
            error = function(e) {
              message(paste("Packet", i, "instrument", e))
              return(NA)
            }
          ),
          "inttime" = .x$inttime,
          "date" = .x$date,
          "sampledelay" = .x$sampledelay,
          "sn" = tryCatch(
            as.character(.x$sn),
            error = function(e) {
              message(paste("Packet", i, "sn", e))
              return(NA)
            }
          ),
          "spectemp" = tryCatch(
            as.character(.x$spectemp),
            error = function(e) {
              message(paste("Packet", i, "spectemp", e))
              return(NA)
            }
          ),
          "timer" = tryCatch(
            as.character(.x$timer),
            error = function(e) {
              message(paste("Packet", i, "timer", e))
              return(NA)
            }
          )
        )
      },
      error = function(e) {
        message(paste("Packet", i, "instrument", e))
        return(NA)
      }
    )
  })

  if (any(is.na(hocr_raw))) {
    na_packet <- which(is.na(hocr_raw))
    hocr_raw <- hocr_raw[-which(is.na(hocr_raw))]
  } else {
    na_packet <- 0
  }

  # check for invalid packet
  ValidInd <- purrr::map_lgl(
    hocr_raw,
    ~ ifelse(
      is.na(.x$instrument),
      F,
      str_detect(.x$instrument, "SAT(HPE|PED|HSE|HED|HPL|PLD|HSL|HLD)")
    )
  )

  if (any(!ValidInd)) {
    message("Invalid HOCR packets detected and removed: ", length(which(!ValidInd)) + na_packet)

    hocr_raw <- hocr_raw[ValidInd]
  }

  return(hocr_raw)
}

#' filter_hocr
#'
#' @description time filter of the \code{hocr_raw} list (from  \code{\link{read_mte_hocr}}) based on time interval.
#' HOCRtimeIndex is precomputed as it takes time.
#'
#' @return Return a subset of \code{hocr_raw}
#'
#' @noRd
filter_hocr <- function(hocr_raw, HOCRtimeIndex, timeInt) {

  ix <- purrr::map_lgl(
    .x = as.numeric(HOCRtimeIndex),
    ~ .x >= as.numeric(int_start(timeInt)) - 1 & .x <= as.numeric(int_end(timeInt))
  )

  hocr_raw[ix]
}

#' tidy_hocr
#'
#' @description tidyer for HOCR packets.
#'
#' @param Packets list of HOCR packets generated by \code{\link{read_mte_hocr}}
#' @param date The binary date format added by the MTE datalogger is unknown.
#' Quick Fix
#'
#' @return Return a long format tidy tibble of the HOCR packets
#'
#' @noRd
tidy_hocr <- function(Packets, date) {
  # If HOCR log with SatView, use there date time format
  # if (!inherits(date, "date") && date == "data") {

  if (any(str_detect(names(Packets), "^date$"))) {
    Year <- str_extract(Packets$date, "^[:digit:]{4}")

    DOY <- str_extract(Packets$date, "[:digit:]{3}$")

    date <- as.date(as.numeric(DOY) - 1, origin = as.date(paste0(Year, "-01-01")))

    time <- substring(Packets$time, c(1, 3, 5, 7), c(2, 4, 6, 9))

    HMS <- str_c(time[1:3], collapse = ":")
    HMSmmm <- str_c(HMS, time[4], sep = ".")

    tibble::tibble(
      # Applanix time added by the DataLogger in millisecond
      # Unkown date format in the binary file, so take the one in the txt file
      time = ymd_hms(paste(date, HMSmmm)),

      # HOCR Packets
      # Fix missing byte bug by ignoring decoding error
      instrument = as.character(Packets$instrument, errors = "ignore"),
      sn = as.character(Packets$sn, errors = "ignore"),
      integration_time = Packets$inttime,
      sample_delay = Packets$sampledelay,
      dark_sample = Packets$darksample,
      dark_average = Packets$darkaverage,
      spectrometer_temperature = as.character(Packets$spectemp, errors = "ignore"),
      frame = Packets$frame,
      timer = as.character(Packets$timer, errors = "ignore"),
      checksum = Packets$checksum,
      channel = Packets$channel
    )
  } else { # If HOCR loged with MTE Data Logger

    tibble::tibble(
      # Applanix time added by the DataLogger in millisecond
      # Unkown date format in the binary file, so take the one in the txt file
      time = as.POSIXct(paste0(date, hms::as_hms(Packets$time / 1000)), format = "%Y-%m-%d %H:%M:%OS", tz = "utc"),

      # HOCR Packets
      # Fix missing byte bug by ignoring decoding error
      instrument = as.character(Packets$instrument, errors = "ignore"),
      sn = as.character(Packets$sn, errors = "ignore"),
      integration_time = Packets$inttime,
      sample_delay = Packets$sampledelay,
      dark_sample = Packets$darksample,
      dark_average = Packets$darkaverage,
      spectrometer_temperature = as.character(Packets$spectemp, errors = "ignore"),
      frame = Packets$frame,
      timer = as.character(Packets$timer, errors = "ignore"),
      checksum = Packets$checksum,
      channel = Packets$channel
    )
  }
}

#' cal_inttime
#'
#' @description Calibrate HOCR integration time
#'
#' @noRd
cal_inttime <- function(raw_data, INTTIME) {
  a0 <- INTTIME$a0
  a1 <- INTTIME$a1

  purrr::modify_in(raw_data, .where = "integration_time", ~ (a0 * .x^0) + (a1 * .x^1))
}

#' cal_optic3
#'
#' @description Calibrate HOCR optical channels
#'
#' @noRd
cal_optic3 <- function(.x, instrument) {
  # In WISEMan 2019, in air calibration file HSL and HLD where used to calibrate the 0237
  # and 0238 HOCR in water.
  # This lead to a situation here where the wrong equation (immersion factor) would be applied
  # I don't know how to deal with such peculiarities and I don't need too
  # (maybe with the "type" column) Future self or others, have fun !

  if (str_detect(instrument, "HSE|HED")) { # In air

    dplyr::mutate(.data = .x, channel = 1.0 * a1 * (channel - a0) * (cint / integration_time))
  } else if (str_detect(instrument, "HPE|PED|HPL|PLD|HSL|HLD")) { # In water (HSL and HLD are normaly in water)

    dplyr::mutate(.data = .x, channel = im * a1 * (channel - a0) * (cint / integration_time))
  } else {
    warning(paste0("instrument name not valid: ", instrument))
  }
}

#' approx_tbl
#'
#' @description approximate measurements on common time coordinates
#'
#' @noRd
approx_tbl <- function(., timeSeq) {
  tbl <- tibble(date_time = timeSeq)

  for (i in seq_along(colnames(.))[-1]) {
    coord <- approx(x = .[[1]], y = .[[i]], xout = timeSeq, method = "linear")

    tbl <- bind_cols(tbl, x = coord[[2]])
    colnames(tbl)[i] <- colnames(.)[i]
  }

  tbl %>%
    mutate(
      id = seq_along(timeSeq),
      qc = "1",
      .before = date_time
    ) %>%
    mutate(
      date_time = as.character(format(date_time, "%Y-%m-%d %H:%M:%S"))
    )
}

#' cal_dark
#'
#' @description Calibrate HOCR dark packets to L1b, is mostly a copy and paste
#' of \code{\link{hocr_l1b}}), should improve that ...
#'
#' @return Return a wide data frame for HOCR dark offset
#'
#' @noRd
cal_dark <- function(hocr_raw, hocr_cal, date) {
  raw_data <- purrr::map_df(hocr_raw, ~ tidy_hocr(., date))

  message("calibrating dark")

  # Bind HOCR with Calibration by instrument (shutter mode) -----------------

  # There will be a problem here if some HCOR packet have corrupted instrument and sn.
  # Have to filter and remove those corrupted pakect before, in the tidy_hocr function ?

  glob_cal <- raw_data %>%
    group_by(instrument, sn) %>%
    nest(raw_data = !matches("instrument|sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$OPTIC3, by = c("instrument", "sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$THERM1, by = c("instrument", "sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$INTTIME, by = c("instrument", "sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$SAMPLE, by = c("instrument", "sn"))

  # Add OPTIC3 to raw data --------------------------------------------------

  # taken from: https://gist.github.com/mdlincoln/528a53939538b07ade86
  row_rep <- function(df, n) {
    df[rep(1:nrow(df), times = n), ]
  }

  glob_cal <- glob_cal %>% mutate(raw_data = purrr::map2(raw_data, OPTIC3, ~ bind_cols(.x, row_rep(.y, nrow(.x) / nrow(.y)))))

  # Calibrate time ----------------------------------------------------------

  glob_cal <- glob_cal %>% mutate(cal_data = purrr::map2(.x = raw_data, .y = INTTIME, .f = ~ cal_inttime(.x, .y)))

  # Calibrate optical channels ----------------------------------------------

  glob_cal <- glob_cal %>% mutate(cal_data = purrr::map2(.x = cal_data, .y = instrument, ~ cal_optic3(.x, .y)))

  # Interpolate time coordinate ---------------------------------------------

  hocr_long <- glob_cal %>% # OPTIC3
    mutate(cal_data = purrr::map(
      cal_data,
      ~ select(.x, !all_of(c("units", "field_length", "data_type", "cal_lines", "fit_type", "a0", "a1", "im", "cint")))
    )) %>% # Packet metadata
    mutate(cal_data = purrr::map(
      cal_data,
      ~ select(.x, !all_of(c("sample_delay", "dark_sample", "dark_average", "spectrometer_temperature", "frame", "timer", "checksum")))
    )) %>%
    select(instrument, sn, cal_data)

  # Convert to wide format

  hocr_wide <- hocr_long %>%
    mutate(cal_data = purrr::map(
      cal_data,
      ~ pivot_wider(
        .,
        names_from = all_of(c("type", "wavelength")),
        names_sep = "_",
        values_from = channel
      )
    )) %>%
    mutate(cal_data = purrr::map(cal_data, ~ select(., where(function(x) all(!is.na(x))))))

    hocr_wide <- hocr_wide %>%
    mutate(
      cal_data = purrr::map(
        .x = cal_data,
        ~ .x %>%
          rename(date_time = time) %>%
          mutate(
            id = seq(1, nrow(.x)),
            qc = "1",
            .before = date_time))
    )

  # Compute the time Sequence
#
#   ShortNobs <- hocr_wide %>%
#     mutate(Nobs = purrr::map_dbl(cal_data, ~ length(rownames(.))))
#
#   ShortNobs <- ShortNobs %>%
#     filter(Nobs == min(ShortNobs$Nobs)) %>%
#     unnest(cols = c(cal_data))
#
#   Mintime <- min(ShortNobs$time)
#   # format(Mintime, "%Y-%m-%d %H:%M:%OS3")
#
#   Maxtime <- max(ShortNobs$time)
#   # format(Maxtime, "%Y-%m-%d %H:%M:%OS3")
#
#   timeSeq <- seq.POSIXt(Mintime, Maxtime, by = 60)
#   # format(timeSeq, "%Y-%m-%d %H:%M:%OS3")
#
#   # Interpolate to commom time coordinate
#
#   hocr_wide <- hocr_wide %>%
#     mutate(cal_data = purrr::map(cal_data, ~ approx_tbl(., timeSeq))) #%>%
#     #select(!cal_data)
#
#   hocr_wide <- hocr_wide %>%
#     mutate(cal_data = purrr::map(cal_data, na.omit))

  hocr_wide <- hocr_wide %>%
    mutate(cal_data = purrr::map(cal_data, ~ select(., !integration_time)))

  return(hocr_wide)
}

#' hocr_l1b
#'
#' @description Calibrate HOCR packets to L1b
#'
#' @param Filthocr_raw subset from \code{\link{filter_hocr}})
#' @param cal_data calibration data from \code{\link{tidy_cal_hocr}}
#' @param cal_data dark data from \code{\link{cal_dark}}
#' @param MainLogdate The binary date format added by the MTE datalogger is unknown.
#' Quick Fix
#'
#' @return Return L1b HOCR data in a tidy long format
#'
#' @noRd
hocr_l1a <- function(hocr_raw, hocr_cal, metadata_l2, UpdateProgress) {
  # If we were passed a progress update function, call it
  if (is.function(UpdateProgress)) {
    text <- "HOCR: "
    UpdateProgress(message = text)
  }

  browser()

  raw_data <- purrr::map_df(hocr_raw, ~ tidy_hocr(., date(metadata_l2$date_time)))

  if (length(unique(raw_data$sn)) < 3) {
    stop(
      paste0("Missing instrument(s), detected only: ",
             paste(unique(raw_data$sn), collapse = ", ")
             )
      )
  }

  # Bind HOCR with Calibration by instrument (shutter mode) -----------------

  glob_cal <- raw_data %>%
    group_by(instrument, sn) %>%
    nest(raw_data = !matches("instrument|sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$OPTIC3, by = c("instrument", "sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$THERM1, by = c("instrument", "sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$INTTIME, by = c("instrument", "sn"))

  glob_cal <- left_join(glob_cal, hocr_cal$SAMPLE, by = c("instrument", "sn"))

  return(glob_cal)
}

hocr_l1b <- function(HOCRDark, WaveSeq) {

  if (is.function(UpdateProgress)) {
    text <- "add OPTIC3"
    UpdateProgress(detail = text)
  }

  # Add OPTIC3 to raw data --------------------------------------------------
  row_rep <- function(df, n) {
    df[rep(1:nrow(df), times = n), ]
  }

  glob_cal <- glob_cal %>% mutate(raw_data = purrr::map2(raw_data, OPTIC3, ~ bind_cols(.x, row_rep(.y, nrow(.x) / nrow(.y)))))

  # Calibrate time ----------------------------------------------------------

  if (is.function(UpdateProgress)) {
    detail <- "calibrate time"
    UpdateProgress(detail = detail)
  }

  glob_cal <- glob_cal %>% mutate(cal_data = purrr::map2(.x = raw_data, .y = INTTIME, .f = ~ cal_inttime(.x, .y)))

  # Calibrate optical channels ----------------------------------------------

  if (is.function(UpdateProgress)) {
    detail <- "calibrate optical channels"
    UpdateProgress(detail = detail)
  }

  glob_cal <- glob_cal %>% mutate(cal_data = purrr::map2(.x = cal_data, .y = instrument, ~ cal_optic3(.x, .y)))

  # Data is at level ProSoft L1b

  # Interpolate time coordinate ---------------------------------------------

  if (is.function(UpdateProgress)) {
    detail <- "Cleaning HOCR"
    UpdateProgress(detail = detail)
  }

  hocr_long <- glob_cal %>% # OPTIC3
    mutate(cal_data = purrr::map(
      cal_data,
      ~ select(.x, !all_of(c("units", "field_length", "data_type", "cal_lines", "fit_type", "a0", "a1", "im", "cint")))
    )) %>% # Packet metadata
    # mutate(cal_data = purrr::map(
    #   cal_data,
    #   ~ select(.x, !all_of(c("sample_delay", "dark_sample", "dark_average", "spectrometer_temperature", "frame", "timer", "checksum")))
    # )) %>%
    select(instrument, sn, cal_data) %>%
    filter(str_detect(instrument, "HPE|HSE|HPL|HSL"))

  # If not 3 instrument HSE|HPL record raise an error

  if (nrow(hocr_long) != 3) {
    warning("if no HOCR data have been recorded for a long time, there is not even dark and therfore no missing light sn to find ...")
    missing_sn <- unique(hocr_cal$OPTIC3$sn)[which(!unique(hocr_cal$OPTIC3$sn) %in% hocr_long$sn)]
    stop(paste("No light record for instrument:", missing_sn))
  }

  # Convert to wide format

  hocr_wide <- hocr_long %>%
    mutate(cal_data = purrr::map(
      cal_data,
      ~ pivot_wider(
        .,
        names_from = all_of(c("type", "wavelength")),
        names_sep = "_",
        values_from = channel
      )
    )) %>%
    mutate(cal_data = purrr::map(cal_data, ~ select(., where(function(x) all(!is.na(x))))))

  # Compute the time Sequence for interpolation with start and end taken from the DataLogger time
  # The interval is fixed at 1 second as this is the rate of data output by the Applanix

  # ShortNobs <- hocr_wide %>%
  #   mutate(Nobs = purrr::map_dbl(cal_data, ~ length(rownames(.))))
  #
  # ShortNobs <- ShortNobs %>%
  #   filter(Nobs == min(ShortNobs$Nobs)) %>%
  #   unnest(cols = c(cal_data))
  #
  # Mintime <- min(ShortNobs$GPStime)
  # # format(Mintime, "%Y-%m-%d %H:%M:%OS3")
  #
  # Maxtime <- max(ShortNobs$GPStime)
  # # format(Maxtime, "%Y-%m-%d %H:%M:%OS3")

  # timeSeq <- seq.POSIXt(min(ymd_hms(metadata_l1b$date_time)), max(ymd_hms(metadata_l1b$date_time)), by = 1)
  # format(timeSeq, "%Y-%m-%d %H:%M:%OS3")

  # Interpolate to commom time coordinates

  # hocr_wide <- hocr_wide %>%
  #   mutate(cal_data = purrr::map(cal_data, ~ select(., !integration_time)))

  # Debug for NA values in interpolation

  if (any(purrr::map_lgl(hocr_wide$cal_data, ~ !nrow(.) > 1))) {
    missing_sn <- hocr_wide$sn[purrr::map_lgl(hocr_wide$cal_data, ~ !nrow(.) > 1)]
    stop(glue::glue("Cannot process with one light record for instrument: ", paste0(missing_sn, collapse = ", ")))
  }

  # Need to test if two non-NA values are available to interpolate
  # This is handled by wrapping hocr_l1b in spsComps::shinyCatch

  # hocr_wide <- hocr_wide %>%
  #   mutate(AproxData = purrr::map(cal_data, ~ approx_tbl(., timeSeq)))

  # Data is at level L2s -------------------------------------------------------

  # Apply dark correction

  if (is.function(UpdateProgress)) {
    detail <- "dark correction"
    UpdateProgress(detail = detail)
  }

  hocr_wide <- left_join(hocr_wide, HOCRDark, by = c("sn"))

  cor_dark <- function(light, dark) {

    data <- light %>%
      select(!matches("[[:alpha:]]*_[[:digit:]]{3}"))

    light <- light %>%
      select(matches("[[:alpha:]]*_[[:digit:]]{3}"))

    dark <- dark %>%
      select(matches("[[:alpha:]]*_[[:digit:]]{3}"))

    if (length(dark) != length(light)) {
      stop("dark and light have different number of wavelength")
    } else {
      light <- light - row_rep(dark, nrow(light))
    }

    return(bind_cols(data, light))
  }

  hocr_wide <- hocr_wide %>%
    mutate(
      cal_data = purrr::map(
        .x = cal_data,
        ~ .x %>%
          rename(date_time = time) %>%
          mutate(
            id = seq(1, nrow(.x)),
            .before = date_time))
    )

  hocr_wide <- hocr_wide %>%
    mutate(cal_data = purrr::map2(cal_data, dark_cal_data, cor_dark))

  # Transform back to long format

  if (is.function(UpdateProgress)) {
    detail <- "long time no sea"
    UpdateProgress(detail = detail)
  }

  hocr_long <- hocr_wide %>%
    mutate(cal_data = purrr::map(
      cal_data,
      ~ pivot_longer(
        .,
        cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
        values_to = "channel",
        names_to = c("type", "wavelength"),
        names_sep = "_",
        # names_prefix = "[[:alpha:]]{2}_",
        names_transform = list(wavelength = as.numeric)
      ) %>%
        group_by(id)
    ))

  # cal_data is not needed further
  hocr_long <- hocr_long %>%
    select(!dark_cal_data)

  ### Approx wavelength
  # L1bAveragelong <- L1bDataWide %>%
  #   mutate(AproxData = purrr::map(
  #     AproxData,
  #     ~ pivot_longer(
  #       .,
  #       cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
  #       values_to = "channel",
  #       names_to = c("type", "wavelength"),
  #       names_sep = "_",
  #       # names_prefix = "[[:alpha:]]{2}_",
  #       names_transform = list(wavelength = as.numeric)
  #     )
  #   ))

  # This parameter should be an user input
  # WaveSeq <- seq(353, 800, 3)

  approx_wave <- function(df, WaveSeq) {

    data <- df %>%
      select(!all_of(c("wavelength", "channel"))) %>%
      unique()

    coord <- approx(x = df$wavelength, y = df$channel, xout = WaveSeq, method = "linear")

    tbl <- tibble(
      data,
      wavelength = coord[[1]],
      channel = coord[[2]]
    )

    return(tbl)
  }

  l1b_approx_long <- hocr_long %>%
    mutate(
      cal_data = purrr::map(
        .x = cal_data,
        ~ .x %>%
          na.omit() %>%
          group_by(id) %>%
          nest(.key = "Nest"))
    )

  l1b_approx_long <- l1b_approx_long %>%
    mutate(
      cal_data = purrr::map(
        cal_data, ~ purrr::map2_df(
          .x = .$id, .y = .$Nest,
          .f = ~ bind_cols(id = .x, approx_wave(.y, WaveSeq))
        )
      )
    )


# filter median +-  1.96 * sd (95% conf) ----------------------------------

  df <- l1b_approx_long %>%
    ungroup() %>%
    unnest(cols = c(cal_data)) %>%
    ungroup()

  df_stats <- df %>%
    group_by(sn, wavelength) %>%
    select(sn, wavelength, channel) %>%
    summarise(
      across(where(is.numeric), list(median = median, sd = ~ sd(.x, na.rm=T)), .names= "{.col}_{.fn}")
    ) %>%
    ungroup()

  # browser()
  #
  # l1b_approx_long <- left_join(
  #   df, df_stats, by = c("sn", "wavelength")
  # ) %>%
  #   mutate(
  #     qc =if_else(
  #         channel > channel_median + 2*channel_sd |
  #               channel < channel_median - 2*channel_sd,
  #         "0",
  #         "1"
  #       )
  #   )

  l1b_approx_long <- left_join(
    df, df_stats, by = c("sn", "wavelength")
  ) %>%
    group_by(instrument, sn, id) %>%
    nest() %>%
    mutate(
      qc = purrr::map_chr(
        .x = data,
        ~ if_else(
        any(
          # .x$channel > .x$channel_median + 1.96*.x$channel_sd |
          # .x$channel < .x$channel_median - 1.96*.x$channel_sd
          F
          ),
        "0",
        "1"
      )
    ),
    .after = id
    ) %>%
    unnest(cols = c(data)) %>%
    select(
      !all_of(c("channel_median", "channel_sd"))
    ) %>%
    group_by(instrument, sn) %>%
    nest(.key = "cal_data")

  return(l1b_approx_long)
}

#' hocr_l2
#'
#' @description Compute L2 AOPs parameter from HOCR
#'
#' @param L1bData
#'
#' @return Tidy long tiblle with Rrs and KLu
#'
#' @noRd
hocr_l2 <- function(L1bData, wave_seq, z1, z2z1,
                    Loess, Span, Obs) {

  hocr_pdf <- function(df) {

    hocr_stats <- df %>%
      group_by(sn, wavelength) %>%
      select(sn, wavelength, channel) %>%
      summarise(
        across(where(is.numeric), list(median = median, sd = ~ sd(.x, na.rm=T)), .names= "{.col}_{.fn}")
      )

    return(hocr_stats)
  }

  df <- L1bData %>%
    unnest(cols = c(cal_data)) %>%
    filter(
      qc == 1
    )

# Remove negative value spectrum ------------------------------------------

  # df <- df %>%
  #   group_by(sn, id) %>%
  #   nest() %>%
  #   mutate(
  #     test = purrr::map(.x = data,
  #                       ~ !any(.x$channel < 0))
  #   ) %>%
  #   filter(
  #     test == T
  #   ) %>%
  #   select(data) %>%
  #   unnest(cols = c(data))


# Smooth input spectrum ---------------------------------------------------
# Usefull to reduce noise in KLu(700 nm >)

  # if (Loess) {
  #
  #   df <- df %>%
  #     group_by(sn, id) %>%
  #     nest() %>%
  #     mutate(
  #       channels_loess = purrr::map(
  #         .x = data,
  #         function(.x) {
  #           func <- loess(
  #             channel ~ wavelength,
  #             data = .x,
  #             na.action = "na.omit",
  #             span = Span
  #           )
  #
  #           .x %>%
  #             mutate(channel = predict(func, wavelength))
  #         }
  #       )
  #     ) %>%
  #     select(channels_loess) %>%
  #     unnest(cols = c(channels_loess))
  # }

# DEV ---------------------------------------------------------------------
  #
  # ply <- test %>%
  #   filter(sn == 1413) %>%
  #   plot_ly(x = ~wavelength) %>%
  #   add_lines(y =~channel)
  #
  # ply

  # ply <- df_pdf %>%
  #   filter(sn == 1396) %>%
  #   plot_ly(x = ~wavelength, y =~channel_median) %>%
  #   add_lines() %>%
  #   add_ribbons(ymin = ~channel_median-channel_sd, ymax = ~channel_median+channel_sd)
  #
  # ply
  #
  # ply <- df_pdf %>%
  #   filter(sn == 1413) %>%
  #   plot_ly(x = ~wavelength, y =~channel_median) %>%
  #   add_lines() %>%
  #   add_ribbons(ymin = ~channel_median-channel_sd, ymax = ~channel_median+channel_sd)
  #
  # ply
  #
  # ply <- df_pdf %>%
  #   filter(sn == 1414) %>%
  #   plot_ly(x = ~wavelength, y =~channel_median) %>%
  #   add_lines() %>%
  #   add_ribbons(ymin = ~channel_median-channel_sd, ymax = ~channel_median+channel_sd)
  #
  # ply

  # QWIP <- qc_qwip(Waves = Rrslong$wavelength, Rrs = Rrslong$Rrs)
  #
  # # Rrslong <- Rrslong %>%
  # #   mutate(
  # #     qwip_score = QWIP$Score
  # #   )
  #
  # Obs$metadata_l2 <- Obs$metadata_l2 %>%
  #   mutate(
  #     qwip_score = QWIP$Score
  #   )

# END DEV -----------------------------------------------------------------

  # df_pdf <- hocr_pdf(df)
  #
  # es <- df_pdf %>%
  #   filter(sn %in% c(1397, 1396, 0341))
  #
  # luz1 <- df_pdf %>%
  #   filter(sn %in% c(1415, 1413, 0237))
  #
  # luz2 <- df_pdf %>%
  #   filter(sn %in% c(1416, 1414, 0238))
  #
  # wavelength <- wave_seq
  #
  # rrs <- propagate_uncertainty(wave_seq, es, luz1, luz2, z1, z2z1, n_draws = 10^5)

  propagate_uncertainty <- function(wavelength, es, luz1, luz2, z1, z2z1, n_draws = 10^4) {

    set.seed(1234)

    # Draw the samples from the PDFs of the input quantity
    luz1_samples <- purrr::map2(
      .x = luz1$channel_median,
      .y = luz1$channel_sd,
      ~ rnorm(
        n_draws, .x, .y
      )
    )
    luz1_samples <- matrix(unlist(luz1_samples), nrow = n_draws, byrow = FALSE)

    luz2_samples <- purrr::map2(
      .x = luz2$channel_median,
      .y = luz2$channel_sd,
      ~ rnorm(
        n_draws, .x, .y
      )
    )
    luz2_samples <- matrix(unlist(luz2_samples), nrow = n_draws, byrow = FALSE)

    # Compute klu, lw, rrs with combined uncertainty

    compute_klu <- function(luz1, luz2, z2z1 = 0.15) {

      klu_samples <- (log(luz1) - log(luz2)) / z2z1

      klu_stats <- purrr::map_dfr(
        seq_along(wavelength), ~ {
          tibble(
            klu_mean = mean(klu_samples[, .x], na.rm = T),
            klu_sd = sd(klu_samples[, .x], na.rm = T)
          )
        })

      return(list(tibble(wavelength, klu_stats), klu_samples))
    }

    klu <- compute_klu(luz1_samples, luz2_samples)
    klu_estimates <- klu[[1]]
    klu_samples <- klu[[2]]

    klu_mean_loess <- function(., wavelength) {
      func <- loess(
        klu_mean ~ wavelength,
        data = .,
        na.action = "na.omit",
        span = 0.18
      )

      predict(func, wavelength)
    }

    klu_sd_loess <- function(., wavelength) {
      func <- loess(
        klu_sd ~ wavelength,
        data = .,
        na.action = "na.omit",
        span = 0.18
      )

      predict(func, wavelength)
    }

    klu_estimates <- klu_estimates %>%
      mutate(
        klu_mean_loess = klu_mean_loess(., wavelength),
        klu_sd_loess = klu_sd_loess(., wavelength)
      )

    klu_samples <- purrr::map2(
      .x = klu_estimates$klu_mean_loess,
      .y = klu_estimates$klu_sd_loess,
      ~ rnorm(
        n_draws, .x, .y
      )
    )
    klu_samples <- matrix(unlist(klu_samples), nrow = n_draws, byrow = FALSE)

    browser()

    compute_lw <- function(luz1, klu, z1) {

      # z1 should be positive
      lw_samples <- 0.54 * luz1 * exp(klu * abs(z1))

      lw_stats <- purrr::map_dfr(
        seq_along(wavelength), ~ {
          tibble(
            lw_mean = mean(lw_samples[, .x], na.rm = T),
            lw_sd = sd(lw_samples[, .x], na.rm = T)
          )
        })

      return(list(tibble(wavelength, lw_stats), lw_samples))
    }

    z1_samples <- matrix(
      rep(
        rnorm(n_draws, z1$z1_median, z1$z1_sd),
        length(wavelength)
      ),
      nrow = n_draws, byrow = F)

    lw <- compute_lw(luz1_samples, klu_samples, z1_samples)
    lw_estimates <- lw[[1]] %>%
      left_join(klu_estimates, by = "wavelength")
    lw_samples <- lw[[2]]

    compute_rrs <- function(lw, es) {
      rrs_samples <- lw/es

      rrs_stats <- purrr::map_dfr(
        seq_along(wavelength), ~ {
          tibble(
            rrs_mean = mean(rrs_samples[, .x], na.rm = T),
            rrs_sd = sd(rrs_samples[, .x], na.rm = T)
          )
        })

      return(tibble(wavelength, rrs_stats))
    }

    es_samples <- purrr::map2(
      .x = es$channel_median,
      .y = es$channel_sd,
      ~ rnorm(
        n_draws, .x, .y
      )
    )
    es_samples <- matrix(unlist(es_samples), nrow = n_draws, byrow = FALSE)

    rrs <- compute_rrs(lw_samples, es_samples) %>%
      left_join(lw_estimates, by = "wavelength")

    ####### Compute component uncertainty to KLu

    compute_rel_unc <- function(base_sd, perturbed_sd) {
      # We compute on variance as it's additive
      return ((base_sd^2 - perturbed_sd^2) / base_sd^2)
    }

    luz1_rel_samples <- matrix(rep(luz1$channel_median, n_draws), nrow = n_draws, byrow = T)

    klu_luz1_rel <- compute_klu(luz1_rel_samples, luz2_samples)
    klu_luz1_rel_estimates <- klu_luz1_rel[[1]]
    klu_luz1_rel_samples <- klu_luz1_rel[[2]]

    luz2_rel_samples <- matrix(rep(luz2$channel_median, n_draws), nrow = n_draws, byrow = T)

    klu_luz2_rel <- compute_klu(luz1_samples, luz2_rel_samples)
    klu_luz2_rel_estimates <- klu_luz2_rel[[1]]
    klu_luz2_rel_samples <- klu_luz2_rel[[2]]

    rrs <- rrs %>%
      mutate(
        klu_luz1_rel_unc = compute_rel_unc(klu_sd, klu_luz1_rel_estimates$klu_sd),
        klu_luz2_rel_unc = compute_rel_unc(klu_sd, klu_luz2_rel_estimates$klu_sd),
        klu_rel_unity = klu_luz1_rel_unc + klu_luz2_rel_unc
      )

    ####### Compute component uncertainty to Lw

    lw_luz1_rel <- compute_lw(luz1_samples, klu_samples, z1_samples)
    lw_luz1_rel_estimates <- lw_luz1_rel[[1]]
    lw_luz1_rel_samples <- lw_luz1_rel[[2]]

    klu_rel_samples <- matrix(rep(klu_estimates$klu_mean_loess, n_draws), nrow = n_draws, byrow = T)

    lw_klu_rel <- compute_lw(luz1_samples, klu_rel_samples, z1_samples)
    lw_klu_rel_estimates <- lw_klu_rel[[1]]
    lw_klu_rel_samples <- lw_klu_rel[[2]]

    z1_rel_samples <- matrix(
      rep(rep(z1$z1_median, length(wavelength)), n_draws),
      nrow = n_draws, byrow = F)

    lw_z1_rel <- compute_lw(luz1_samples, klu_samples, z1_rel_samples)
    lw_z1_rel_estimates <- lw_z1_rel[[1]]
    lw_z1_rel_samples <- lw_z1_rel[[2]]

    rrs <- rrs %>%
      mutate(
        lw_luz1_rel_unc = compute_rel_unc(lw_sd, lw_luz1_rel_estimates$lw_sd),
        lw_klu_rel_unc = compute_rel_unc(lw_sd, lw_klu_rel_estimates$lw_sd),
        lw_z1_rel_unc = compute_rel_unc(lw_sd, lw_z1_rel_estimates$lw_sd),
        lw_rel_unity = lw_luz1_rel_unc + lw_klu_rel_unc + lw_z1_rel_unc
      )

    ####### Compute component uncertainty to Rrs

    lw_rel_samples <- matrix(rep(lw_estimates$lw_mean, n_draws), nrow = n_draws, byrow = T)

    rrs_lw_rel <- compute_rrs(lw_rel_samples, es_samples)
    # rrs_lw_rel_estimates <- rrs_lw_rel[[1]]
    # rrs_lw_rel_samples <- rrs_lw_rel[[2]]

    es_rel_samples <- matrix(rep(es$channel_median, n_draws), nrow = n_draws, byrow = T)

    rrs_es_rel <- compute_rrs(lw_samples, es_rel_samples)
    # rrs_es_rel_estimates <- rrs_es_rel[[1]]
    # rrs_es_rel_samples <- rrs_es_rel[[2]]

    rrs <- rrs %>%
      mutate(
        rrs_lw_rel_unc = compute_rel_unc(rrs_sd, rrs_lw_rel$rrs_sd),
        rrs_es_rel_unc = compute_rel_unc(rrs_sd, rrs_es_rel$rrs_sd),
        rrs_rel_unity = rrs_lw_rel_unc + rrs_es_rel_unc
      )

    return(rrs)
  }

  df_pdf <- hocr_pdf(df)

  es <- df_pdf %>%
    filter(as.character(sn) %in% c("1397", "1396", "0341"))

  luz1 <- df_pdf %>%
    filter(as.character(sn) %in% c("1415", "1413", "0237"))

  luz2 <- df_pdf %>%
    filter(as.character(sn) %in% c("1416", "1414", "0238"))

  rrs <- propagate_uncertainty(wave_seq, es, luz1, luz2, z1, z2z1)

# DEV ---------------------------------------------------------------------
#
#   ply <- rrs %>%
#     plot_ly(x = ~wavelength, y = ~klu_mean) %>%
#     add_lines() %>%
#     add_ribbons(ymin = ~klu_mean-klu_sd, ymax = ~klu_mean+klu_sd)
#
#   ply
#
#   ply <- rrs %>%
#     plot_ly(x = ~wavelength, y = ~rrs_mean) %>%
#     add_lines() %>%
#     add_ribbons(ymin = ~rrs_mean-rrs_sd, ymax = ~rrs_mean+rrs_sd)
#
#   ply
#
#   ply <- rrs %>%
#     plot_ly(x = ~ wavelength) %>%
#     add_trace(
#       type = 'scatter', mode = 'lines', fill = 'tonexty',
#       y = ~ rrs_sd * rrs_luz1_rel_unc ,
#       name = "unc_luz1",
#       line = list(color = 'rgba(105, 159, 245, 0.5)'),
#       fillcolor = 'rgba(105, 159, 245, 0.3)',
#       legendgroup = 1,
#       showlegend = T
#     ) %>%
#     add_trace(
#       type = 'scatter', mode = 'lines', fill = 'tonexty',
#       y = ~
#         rrs_sd * rrs_luz1_rel_unc +
#         rrs_sd * rrs_luz2_rel_unc ,
#       name = "unc_luz2",
#       line = list(color = 'rgba(6, 58, 143, 0.5)'),
#       fillcolor = 'rgba(6, 33, 143, 0.3)',
#       legendgroup = 1,
#       showlegend = T
#     ) %>%
#     add_trace(
#       type = 'scatter', mode = 'lines', fill = 'tonexty',
#       y = ~
#         rrs_sd * rrs_luz1_rel_unc +
#         rrs_sd * rrs_luz2_rel_unc +
#         rrs_sd * rrs_z1_rel_unc,
#       name = "unc_z1",
#       line = list(color = 'rgba(74, 176, 100, 0.5)'),
#       fillcolor = 'rgba(74, 176, 100, 0.3)',
#       legendgroup = 1,
#       showlegend = T
#     ) %>%
#     add_trace(
#       type = 'scatter', mode = 'lines', fill = 'tonexty',
#       y = ~
#         rrs_sd * rrs_luz1_rel_unc +
#         rrs_sd * rrs_luz2_rel_unc +
#         rrs_sd * rrs_z1_rel_unc +
#         rrs_sd * rrs_es_rel_unc,
#       name = "unc_es",
#       line = list(color = 'rgba(169, 74, 176, 0.5)'),
#       fillcolor = 'rgba(169, 74, 176, 0.3)',
#       legendgroup = 1,
#       showlegend = T
#     ) %>%
#     layout(
#       xaxis = list(title=TeX("\\text{wavelength}")),
#       yaxis = list(title=TeX("R_\\text{rs} [\\text{sr}^{-1}]"))
#     ) %>%
#     config(mathjax = "cdn", displayModeBar = F)
#
#   ply

# END DEV -----------------------------------------------------------------

  l2_data <- rrs

  # L1bDataWide <- L1bData %>%
  #   mutate(cal_data = purrr::map(
  #     cal_data,
  #     ~ pivot_wider(
  #       .,
  #       names_from = all_of(c("type", "wavelength")),
  #       names_sep = "_",
  #       values_from = channel
  #     ) %>%
  #       ungroup()
  #   )) %>%
  #   ungroup()
  #
  # L1bDataWide <- L1bDataWide %>%
  #   mutate(cal_data = purrr::map(cal_data, ~ filter(., qc == "1")))
  #
  # L1bDataWide <- L1bDataWide %>%
  #   mutate(cal_data = purrr::map(cal_data, ~ summarise(.x, across(.cols = !matches("id|qc|date_time"), ~ mean(.x, na.rm = T)))))
  #
  # Es <- L1bDataWide %>%
  #   filter(sn %in% "1397" | sn == "1396" | sn == "0341") %>%
  #   unnest(cols = c(cal_data)) %>%
  #   select(!matches("instrument|sn|date_time|uuid_l2"))
  #
  # LuZ1 <- L1bDataWide %>%
  #   filter(sn == "1415" | sn == "1413" | sn == "0237") %>%
  #   unnest(cols = c(cal_data)) %>%
  #   select(!matches("instrument|sn|date_time|uuid_l2"))
  #
  # LuZ2 <- L1bDataWide %>%
  #   filter(sn == "1416" | sn == "1414" | sn == "0238") %>%
  #   unnest(cols = c(cal_data)) %>%
  #   select(!matches("instrument|sn|date_time|uuid_l2"))
  #
  # # Z1Z2Depth <- 0.15 # Algae Wise 2022
  #
  # # Consider suppressWarnings(expr)
  #
  # KLuWide <- suppressWarnings((log(LuZ1) - log(LuZ2)) / Z1Z2Depth)
  #
  # KLuWide <- rename_with(KLuWide, ~ str_replace(.x, "LU", "KLu"))
  #
  # KLulong <- KLuWide %>%
  #   pivot_longer(
  #     .,
  #     cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
  #     values_to = "KLu",
  #     names_to = c("wavelength"),
  #     # names_sep = "_",
  #     names_prefix = "[[:alpha:]]{3}_",
  #     names_transform = list(wavelength = as.numeric)
  #   )
  #
  # if (Loess) {
  #   KLuloess <- loess(
  #     KLu ~ wavelength,
  #     data = KLulong,
  #     na.action = "na.omit",
  #     span = Span
  #   )
  #
  #   KLulong <- KLulong %>%
  #     mutate(
  #       KLu_loess = predict(KLuloess, wavelength)
  #     )
  #
  #   KLuWide <- KLulong %>%
  #     select(wavelength, KLu_loess) %>%
  #     pivot_wider(
  #       names_from = "wavelength",
  #       names_prefix = "KLu_loess_",
  #       names_sep = "_",
  #       values_from = c(KLu_loess)
  #     )
  # }
  #
  # # Z1Depth <- 0.10 # 10 cm
  #
  # # 0.54 is the radiance transmittance for the air/water interface
  # Lw <- 0.54 * LuZ1 * exp(-Z1Depth * KLuWide)
  # RrsWide <- Lw / Es
  #
  # Rrslong <- RrsWide %>%
  #   pivot_longer(
  #     .,
  #     cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
  #     values_to = "Rrs",
  #     names_to = c("wavelength"),
  #     # names_sep = "_",
  #     names_prefix = "[[:alpha:]]{2}_",
  #     names_transform = list(wavelength = as.numeric)
  #   )
  #
  # QWIP <- qc_qwip(Waves = Rrslong$wavelength, Rrs = Rrslong$Rrs)
  #
  # # Rrslong <- Rrslong %>%
  # #   mutate(
  # #     qwip_score = QWIP$Score
  # #   )
  #
  # Obs$metadata_l2 <- Obs$metadata_l2 %>%
  #   mutate(
  #     qwip_score = QWIP$Score
  #   )
  #
  # if (Loess) {
  #   Rrsloess <- loess(
  #     Rrs ~ wavelength,
  #     data = Rrslong,
  #     na.action = "na.omit",
  #     span = Span
  #   )
  #
  #   Rrslong <- Rrslong %>%
  #     mutate(
  #       Rrs_loess = predict(Rrsloess, wavelength)
  #     )
  # }
  #
  # l2_data <- left_join(Rrslong, KLulong, by = "wavelength")

  # RbII computation --------------------------------------------------------

  # if(!is.null(Obs$BioSonic$L2$bottom_elevation_m)) {
  #
  #   Eslong <- l1b_approx_long %>%
  #     select(!AproxData) %>%
  #     filter(sn == "1397" | sn == "1396") %>%
  #     unnest(cols = c(IntData)) %>%
  #     select(!matches("instrument|sn|date_time|cal_data|uuid_l2|type")) %>%
  #     rename(Es = channel)
  #
  #   LuZ2long <- l1b_approx_long %>%
  #     select(!AproxData) %>%
  #     filter(sn == "1416" | sn == "1414") %>%
  #     unnest(cols = c(IntData)) %>%
  #     select(!matches("instrument|sn|date_time|cal_data|uuid_l2|type")) %>%
  #     rename(LuZ2 = channel)
  #
  #   RbII <- left_join(KLulong, Eslong, by = c("wavelength")) %>%
  #     left_join(LuZ2long, by = c("wavelength"))
  #
  #   RbII <- RbII %>%
  #     mutate(
  #       Edb = Es/exp(-KLu_loess*Obs$BioSonic$L2$bottom_elevation_m),
  #       Lub = LuZ2/exp(-KLu_loess*Obs$BioSonic$L2$bottom_elevation_m),
  #       RbII = (pi*Lub)/Edb
  #     ) %>%
  #     select(wavelength, RbII)
  #
  #   l2_data <- l2_data %>%
  #     left_join(RbII, by = "wavelength")
  #
  # }


  # Populate uuid_l2 if exist
  if (any(names(L1bData) == "uuid_l2")) {
    l2_data <- l2_data %>%
      mutate(uuid_l2 = unique(L1bData$uuid_l2))
  }

  return(l2_data)
}
