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

  RawHOCR <- purrr::map(BinFile, HocrMte$from_file)

  RawHOCR <- unlist(purrr::map(RawHOCR, ~ .x$packets))

  # Unefficent way of removing custom class python object dependencies resulting in null pointer
  # How to avoid: Error in py_ref_to_r(x) : Embedded NUL in string ? (packet 27281 of AlgaeValidation (2022/07/05))

  RawHOCR <- purrr::imap(RawHOCR, ~ {
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
              message(paste("Packet", i, "SN", e))
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

  if (any(is.na(RawHOCR))) {
    NaPackets <- which(is.na(RawHOCR))
    RawHOCR <- RawHOCR[-which(is.na(RawHOCR))]
  } else {
    NaPackets <- 0
  }

  # check for invalid packet
  ValidInd <- purrr::map_lgl(
    RawHOCR,
    ~ ifelse(
      is.na(.x$instrument),
      F,
      str_detect(.x$instrument, "SAT(HPE|PED|HSE|HED|HPL|PLD|HSL|HLD)")
    )
  )

  if (any(!ValidInd)) {
    message("Invalid HOCR packets detected and removed: ", length(which(!ValidInd)) + NaPackets)

    RawHOCR <- RawHOCR[ValidInd]
  }

  return(RawHOCR)
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

  RawHOCR <- purrr::map(RawFile, HocrSatview$from_file)

  RawHOCR <- unlist(purrr::map(RawHOCR, ~ .x$packets))

  # Unefficent way of removing custom class python object dependencies resulting in null pointer
  # How to avoid: Error in py_ref_to_r(x) : Embedded NUL in string ? (packet 27281 of AlgaeValidation (2022/07/05))

  RawHOCR <- purrr::imap(.x = RawHOCR, ~ {
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

  if (any(is.na(RawHOCR))) {
    NaPackets <- which(is.na(RawHOCR))
    RawHOCR <- RawHOCR[-which(is.na(RawHOCR))]
  } else {
    NaPackets <- 0
  }

  # check for invalid packet
  ValidInd <- purrr::map_lgl(
    RawHOCR,
    ~ ifelse(
      is.na(.x$instrument),
      F,
      str_detect(.x$instrument, "SAT(HPE|PED|HSE|HED|HPL|PLD|HSL|HLD)")
    )
  )

  if (any(!ValidInd)) {
    message("Invalid HOCR packets detected and removed: ", length(which(!ValidInd)) + NaPackets)

    RawHOCR <- RawHOCR[ValidInd]
  }

  return(RawHOCR)
}

#' filter_hocr
#'
#' @description Time filter of the \code{RawHOCR} list (from  \code{\link{read_mte_hocr}}) based on time interval.
#' HOCRTimeIndex is precomputed as it takes time.
#'
#' @return Return a subset of \code{RawHOCR}
#'
#' @noRd
filter_hocr <- function(RawHOCR, HOCRTimeIndex, TimeInt) {

  Ind <- purrr::map_lgl(
    .x = as.numeric(HOCRTimeIndex),
    ~ .x >= as.numeric(int_start(TimeInt)) - 1 & .x <= as.numeric(int_end(TimeInt))
  )

  RawHOCR[Ind]
}

#' tidy_hocr
#'
#' @description tidyer for HOCR packets.
#'
#' @param Packets list of HOCR packets generated by \code{\link{read_mte_hocr}}
#' @param Date The binary date format added by the MTE datalogger is unknown.
#' Quick Fix
#'
#' @return Return a long format tidy tibble of the HOCR packets
#'
#' @noRd
tidy_hocr <- function(Packets, Date) {
  # If HOCR log with SatView, use there date time format
  # if (!inherits(Date, "Date") && Date == "data") {

  if (any(str_detect(names(Packets), "^date$"))) {
    Year <- str_extract(Packets$date, "^[:digit:]{4}")

    DOY <- str_extract(Packets$date, "[:digit:]{3}$")

    Date <- as.Date(as.numeric(DOY) - 1, origin = as.Date(paste0(Year, "-01-01")))

    Time <- substring(Packets$time, c(1, 3, 5, 7), c(2, 4, 6, 9))

    HMS <- str_c(Time[1:3], collapse = ":")
    HMSmmm <- str_c(HMS, Time[4], sep = ".")

    tibble::tibble(
      # Applanix time added by the DataLogger in millisecond
      # Unkown Date format in the binary file, so take the one in the txt file
      Time = ymd_hms(paste(Date, HMSmmm)),

      # HOCR Packets
      # Fix missing byte bug by ignoring decoding error
      Instrument = as.character(Packets$instrument, errors = "ignore"),
      SN = as.character(Packets$sn, errors = "ignore"),
      IntTime = Packets$inttime,
      SampleDelay = Packets$sampledelay,
      DarkSample = Packets$darksample,
      DarkAverage = Packets$darkaverage,
      SpecTemp = as.character(Packets$spectemp, errors = "ignore"),
      Frame = Packets$frame,
      Timer = as.character(Packets$timer, errors = "ignore"),
      CheckSum = Packets$checksum,
      Channels = Packets$channel
    )
  } else { # If HOCR loged with MTE Data Logger

    tibble::tibble(
      # Applanix time added by the DataLogger in millisecond
      # Unkown Date format in the binary file, so take the one in the txt file
      Time = as.POSIXct(paste0(Date, hms::as_hms(Packets$time / 1000)), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),

      # HOCR Packets
      # Fix missing byte bug by ignoring decoding error
      Instrument = as.character(Packets$instrument, errors = "ignore"),
      SN = as.character(Packets$sn, errors = "ignore"),
      IntTime = Packets$inttime,
      SampleDelay = Packets$sampledelay,
      DarkSample = Packets$darksample,
      DarkAverage = Packets$darkaverage,
      SpecTemp = as.character(Packets$spectemp, errors = "ignore"),
      Frame = Packets$frame,
      Timer = as.character(Packets$timer, errors = "ignore"),
      CheckSum = Packets$checksum,
      Channels = Packets$channel
    )
  }
}

#' cal_inttime
#'
#' @description Calibrate HOCR integration time
#'
#' @noRd
cal_inttime <- function(RawData, INTTIME) {
  a0 <- INTTIME$a0
  a1 <- INTTIME$a1

  purrr::modify_in(RawData, .where = "IntTime", ~ (a0 * .x^0) + (a1 * .x^1))
}

#' cal_optic3
#'
#' @description Calibrate HOCR optical channels
#'
#' @noRd
cal_optic3 <- function(.x, Instrument) {
  # In WISEMan 2019, in air calibration file HSL and HLD where used to calibrate the 0237
  # and 0238 HOCR in water.
  # This lead to a situation here where the wrong equation (immersion factor) would be applied
  # I don't know how to deal with such peculiarities and I don't need too
  # (maybe with the "Type" column) Future self or others, have fun !

  if (str_detect(Instrument, "HSE|HED")) { # In air

    dplyr::mutate(.data = .x, Channels = 1.0 * a1 * (Channels - a0) * (cint / IntTime))
  } else if (str_detect(Instrument, "HPE|PED|HPL|PLD|HSL|HLD")) { # In water (HSL and HLD are normaly in water)

    dplyr::mutate(.data = .x, Channels = im * a1 * (Channels - a0) * (cint / IntTime))
  } else {
    warning(paste0("Instrument name not valid: ", Instrument))
  }
}

#' approx_tbl
#'
#' @description approximate measurements on common time coordinates
#'
#' @noRd
approx_tbl <- function(., TimeSeq) {
  tbl <- tibble(DateTime = TimeSeq)

  for (i in seq_along(colnames(.))[-1]) {
    coord <- approx(x = .[[1]], y = .[[i]], xout = TimeSeq, method = "linear")

    tbl <- bind_cols(tbl, x = coord[[2]])
    colnames(tbl)[i] <- colnames(.)[i]
  }

  tbl %>%
    mutate(
      ID = seq_along(TimeSeq),
      QC = "1",
      .before = DateTime
    ) %>%
    mutate(
      DateTime = as.character(format(DateTime, "%Y-%m-%d %H:%M:%S"))
    )
}

#' cal_dark
#'
#' @description Calibrate HOCR dark packets to L1b, is mostly a copy and paste
#' of \code{\link{cal_hocr}}), should improve that ...
#'
#' @return Return a wide data frame for HOCR dark offset
#'
#' @noRd
cal_dark <- function(RawHOCR, CalHOCR, Date) {
  RawData <- purrr::map_df(RawHOCR, ~ tidy_hocr(., Date))

  # Bind HOCR with Calibration by Instrument (shutter mode) -----------------

  # There will be a problem here if some HCOR packet have corrupted Instrument and SN.
  # Have to filter and remove those corrupted pakect before, in the tidy_hocr function ?

  GlobCal <- RawData %>%
    group_by(Instrument, SN) %>%
    nest(RawData = !matches("Instrument|SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$OPTIC3, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$THERM1, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$INTTIME, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$SAMPLE, by = c("Instrument", "SN"))

  # Add OPTIC3 to raw data --------------------------------------------------

  # taken from: https://gist.github.com/mdlincoln/528a53939538b07ade86
  row_rep <- function(df, n) {
    df[rep(1:nrow(df), times = n), ]
  }

  GlobCal <- GlobCal %>% mutate(RawData = purrr::map2(RawData, OPTIC3, ~ bind_cols(.x, row_rep(.y, nrow(.x) / nrow(.y)))))

  # Calibrate time ----------------------------------------------------------

  GlobCal <- GlobCal %>% mutate(CalData = purrr::map2(.x = RawData, .y = INTTIME, .f = ~ cal_inttime(.x, .y)))

  # Calibrate optical channels ----------------------------------------------

  GlobCal <- GlobCal %>% mutate(CalData = purrr::map2(.x = CalData, .y = Instrument, ~ cal_optic3(.x, .y)))

  # Interpolate time coordinate ---------------------------------------------

  HOCRLong <- GlobCal %>% # OPTIC3
    mutate(CalData = purrr::map(
      CalData,
      ~ select(.x, !all_of(c("Units", "FieldLength", "DataType", "CalLines", "FitType", "a0", "a1", "im", "cint")))
    )) %>% # Packet metadata
    mutate(CalData = purrr::map(
      CalData,
      ~ select(.x, !all_of(c("SampleDelay", "DarkSample", "DarkAverage", "SpecTemp", "Frame", "Timer", "CheckSum")))
    )) %>%
    select(Instrument, SN, CalData)

  # Convert to wide format

  HOCRWide <- HOCRLong %>%
    mutate(CalData = purrr::map(
      CalData,
      ~ pivot_wider(
        .,
        names_from = all_of(c("Type", "Wavelength")),
        names_sep = "_",
        values_from = Channels
      )
    )) %>%
    mutate(CalData = purrr::map(CalData, ~ select(., where(function(x) all(!is.na(x))))))

    HOCRWide <- HOCRWide %>%
    mutate(
      CalData = purrr::map(
        .x = CalData,
        ~ .x %>%
          rename(DateTime = Time) %>%
          mutate(
            ID = seq(1, nrow(.x)),
            QC = "1",
            .before = DateTime))
    )

  # Compute the Time Sequence
#
#   ShortNobs <- HOCRWide %>%
#     mutate(Nobs = purrr::map_dbl(CalData, ~ length(rownames(.))))
#
#   ShortNobs <- ShortNobs %>%
#     filter(Nobs == min(ShortNobs$Nobs)) %>%
#     unnest(cols = c(CalData))
#
#   MinTime <- min(ShortNobs$Time)
#   # format(MinTime, "%Y-%m-%d %H:%M:%OS3")
#
#   MaxTime <- max(ShortNobs$Time)
#   # format(MaxTime, "%Y-%m-%d %H:%M:%OS3")
#
#   TimeSeq <- seq.POSIXt(MinTime, MaxTime, by = 60)
#   # format(TimeSeq, "%Y-%m-%d %H:%M:%OS3")
#
#   # Interpolate to commom time coordinate
#
#   HOCRWide <- HOCRWide %>%
#     mutate(CalData = purrr::map(CalData, ~ approx_tbl(., TimeSeq))) #%>%
#     #select(!CalData)
#
#   HOCRWide <- HOCRWide %>%
#     mutate(CalData = purrr::map(CalData, na.omit))

  HOCRWide <- HOCRWide %>%
    mutate(CalData = purrr::map(CalData, ~ select(., !IntTime)))

  return(HOCRWide)
}

#' cal_hocr
#'
#' @description Calibrate HOCR packets to L1b
#'
#' @param FiltRawHOCR subset from \code{\link{filter_hocr}})
#' @param CalData calibration data from \code{\link{tidy_cal_hocr}}
#' @param CalData dark data from \code{\link{cal_dark}}
#' @param MainLogDate The binary date format added by the MTE datalogger is unknown.
#' Quick Fix
#'
#' @return Return L1b HOCR data in a tidy long format
#'
#' @noRd
cal_hocr <- function(RawHOCR, CalHOCR, HOCRDark, MetadataL2, UpdateProgress, WaveSeq) {
  # If we were passed a progress update function, call it
  if (is.function(UpdateProgress)) {
    text <- "HOCR: "
    UpdateProgress(message = text)
  }

  RawData <- purrr::map_df(RawHOCR, ~ tidy_hocr(., date(MetadataL2$DateTime)))

  if (length(unique(RawData$SN)) < 3) {
    stop(
      paste0("Missing instrument(s), detected only: ",
             paste(unique(RawData$SN), collapse = ", ")
             )
      )
  }

  # Bind HOCR with Calibration by Instrument (shutter mode) -----------------

  GlobCal <- RawData %>%
    group_by(Instrument, SN) %>%
    nest(RawData = !matches("Instrument|SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$OPTIC3, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$THERM1, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$INTTIME, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$SAMPLE, by = c("Instrument", "SN"))

  if (is.function(UpdateProgress)) {
    text <- "add OPTIC3"
    UpdateProgress(detail = text)
  }

  # Add OPTIC3 to raw data --------------------------------------------------

  # taken from: https://gist.github.com/mdlincoln/528a53939538b07ade86
  row_rep <- function(df, n) {
    df[rep(1:nrow(df), times = n), ]
  }


  GlobCal <- GlobCal %>% mutate(RawData = purrr::map2(RawData, OPTIC3, ~ bind_cols(.x, row_rep(.y, nrow(.x) / nrow(.y)))))

  # Calibrate time ----------------------------------------------------------

  if (is.function(UpdateProgress)) {
    detail <- "calibrate time"
    UpdateProgress(detail = detail)
  }

  GlobCal <- GlobCal %>% mutate(CalData = purrr::map2(.x = RawData, .y = INTTIME, .f = ~ cal_inttime(.x, .y)))

  # Calibrate optical channels ----------------------------------------------

  if (is.function(UpdateProgress)) {
    detail <- "calibrate optical channels"
    UpdateProgress(detail = detail)
  }

  GlobCal <- GlobCal %>% mutate(CalData = purrr::map2(.x = CalData, .y = Instrument, ~ cal_optic3(.x, .y)))

  # Data is at level ProSoft L1b

  # Interpolate time coordinate ---------------------------------------------

  if (is.function(UpdateProgress)) {
    detail <- "Cleaning HOCR"
    UpdateProgress(detail = detail)
  }

  HOCRLong <- GlobCal %>% # OPTIC3
    mutate(CalData = purrr::map(
      CalData,
      ~ select(.x, !all_of(c("Units", "FieldLength", "DataType", "CalLines", "FitType", "a0", "a1", "im", "cint")))
    )) %>% # Packet metadata
    # mutate(CalData = purrr::map(
    #   CalData,
    #   ~ select(.x, !all_of(c("SampleDelay", "DarkSample", "DarkAverage", "SpecTemp", "Frame", "Timer", "CheckSum")))
    # )) %>%
    select(Instrument, SN, CalData) %>%
    filter(str_detect(Instrument, "HPE|HSE|HPL|HSL"))

  # If not 3 instrument HSE|HPL record raise an error

  if (nrow(HOCRLong) != 3) {
    warning("if no HOCR data have been recorded for a long time, there is not even dark and therfore no missing light SN to find ...")
    MissSn <- unique(CalHOCR$OPTIC3$SN)[which(!unique(CalHOCR$OPTIC3$SN) %in% HOCRLong$SN)]
    stop(paste("No light record for instrument:", MissSn))
  }

  # Convert to wide format

  HOCRWide <- HOCRLong %>%
    mutate(CalData = purrr::map(
      CalData,
      ~ pivot_wider(
        .,
        names_from = all_of(c("Type", "Wavelength")),
        names_sep = "_",
        values_from = Channels
      )
    )) %>%
    mutate(CalData = purrr::map(CalData, ~ select(., where(function(x) all(!is.na(x))))))

  # Compute the Time Sequence for interpolation with start and end taken from the DataLogger time
  # The interval is fixed at 1 second as this is the rate of data output by the Applanix

  # ShortNobs <- HOCRWide %>%
  #   mutate(Nobs = purrr::map_dbl(CalData, ~ length(rownames(.))))
  #
  # ShortNobs <- ShortNobs %>%
  #   filter(Nobs == min(ShortNobs$Nobs)) %>%
  #   unnest(cols = c(CalData))
  #
  # MinTime <- min(ShortNobs$GPSTime)
  # # format(MinTime, "%Y-%m-%d %H:%M:%OS3")
  #
  # MaxTime <- max(ShortNobs$GPSTime)
  # # format(MaxTime, "%Y-%m-%d %H:%M:%OS3")

  # TimeSeq <- seq.POSIXt(min(ymd_hms(MetadataL1b$DateTime)), max(ymd_hms(MetadataL1b$DateTime)), by = 1)
  # format(TimeSeq, "%Y-%m-%d %H:%M:%OS3")

  # Interpolate to commom time coordinates

  # HOCRWide <- HOCRWide %>%
  #   mutate(CalData = purrr::map(CalData, ~ select(., !IntTime)))

  # Debug for NA values in interpolation

  if (any(purrr::map_lgl(HOCRWide$CalData, ~ !nrow(.) > 1))) {
    MissSn <- HOCRWide$SN[purrr::map_lgl(HOCRWide$CalData, ~ !nrow(.) > 1)]
    stop(glue::glue("Cannot process with one light record for instrument: ", paste0(MissSn, collapse = ", ")))
  }

  # Need to test if two non-NA values are available to interpolate
  # This is handled by wrapping cal_hocr in spsComps::shinyCatch

  # HOCRWide <- HOCRWide %>%
  #   mutate(AproxData = purrr::map(CalData, ~ approx_tbl(., TimeSeq)))

  # Data is at level L2s -------------------------------------------------------

  # Apply dark correction

  if (is.function(UpdateProgress)) {
    detail <- "dark correction"
    UpdateProgress(detail = detail)
  }

  HOCRWide <- left_join(HOCRWide, HOCRDark, by = c("SN"))

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

  HOCRWide <- HOCRWide %>%
    mutate(
      CalData = purrr::map(
        .x = CalData,
        ~ .x %>%
          rename(DateTime = Time) %>%
          mutate(
            ID = seq(1, nrow(.x)),
            QC = "1",
            .before = DateTime))
    )

  HOCRWide <- HOCRWide %>%
    mutate(CalData = purrr::map2(CalData, DarkCalData, cor_dark))

  # Transform back to long format

  if (is.function(UpdateProgress)) {
    detail <- "long time no sea"
    UpdateProgress(detail = detail)
  }

  HOCRLong <- HOCRWide %>%
    mutate(CalData = purrr::map(
      CalData,
      ~ pivot_longer(
        .,
        cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
        values_to = "Channels",
        names_to = c("Type", "Wavelength"),
        names_sep = "_",
        # names_prefix = "[[:alpha:]]{2}_",
        names_transform = list(Wavelength = as.numeric)
      ) %>%
        group_by(ID)
    ))

  # CalData is not needed further
  HOCRLong <- HOCRLong %>%
    select(!DarkCalData)

  ### Approx wavelength
  # L1bAverageLong <- L1bDataWide %>%
  #   mutate(AproxData = purrr::map(
  #     AproxData,
  #     ~ pivot_longer(
  #       .,
  #       cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
  #       values_to = "Channels",
  #       names_to = c("Type", "Wavelength"),
  #       names_sep = "_",
  #       # names_prefix = "[[:alpha:]]{2}_",
  #       names_transform = list(Wavelength = as.numeric)
  #     )
  #   ))

  # This parameter should be an user input
  # WaveSeq <- seq(353, 800, 3)

  approx_wave <- function(df, WaveSeq) {

    data <- df %>%
      select(!all_of(c("Wavelength", "Channels"))) %>%
      unique()

    coord <- approx(x = df$Wavelength, y = df$Channels, xout = WaveSeq, method = "linear")

    tbl <- tibble(
      data,
      Wavelength = coord[[1]],
      Channels = coord[[2]]
    )

    return(tbl)
  }

  L1bAproxLong <- HOCRLong %>%
    mutate(
      CalData = purrr::map(
        .x = CalData,
        ~ .x %>%
          na.omit() %>%
          group_by(ID) %>%
          nest(.key = "Nest"))
    )

  L1bAproxLong <- L1bAproxLong %>%
    mutate(
      CalData = purrr::map(
        CalData, ~ purrr::map2_df(
          .x = .$ID, .y = .$Nest,
          .f = ~ bind_cols(ID = .x, approx_wave(.y, WaveSeq))
        )
      )
    )

  return(L1bAproxLong)
}

#' L2_hocr
#'
#' @description Compute L2 AOPs parameter from HOCR
#'
#' @param L1bData
#'
#' @return Tidy long tiblle with Rrs and KLu
#'
#' @noRd
L2_hocr <- function(L1bData, wave_seq, z1, z2z1,
                    Loess, Span, Obs) {

  hocr_pdf <- function(df) {

    hocr_stats <- df %>%
      group_by(SN, Wavelength) %>%
      select(SN, Wavelength, Channels) %>%
      summarise(
        across(where(is.numeric), list(median = median, sd = ~ sd(.x, na.rm=T)), .names= "{.col}_{.fn}")
      )

    return(hocr_stats)
  }

  df <- L1bData %>%
    unnest(cols = c(CalData)) %>%
    filter(
      QC == 1
    )

  df_pdf <- hocr_pdf(df)

  propagate_uncertainty <- function(wavelength, es, luz1, luz2, z1, z2z1, n_draws = 10^5) {

    # Draw the samples from the PDFs of the input quantity
    es_samples <- purrr::map2(
      .x = es$Channels_median,
      .y = es$Channels_sd,
      ~ rnorm(
        n_draws, .x, .y
      )
    )
    es_samples <- matrix(unlist(es_samples), nrow = n_draws, byrow = FALSE)

    luz1_samples <- purrr::map2(
      .x = luz1$Channels_median,
      .y = luz1$Channels_sd,
      ~ rnorm(
        n_draws, .x, .y
      )
    )
    luz1_samples <- matrix(unlist(luz1_samples), nrow = n_draws, byrow = FALSE)

    luz2_samples <- purrr::map2(
      .x = luz2$Channels_median,
      .y = luz2$Channels_sd,
      ~ rnorm(
        n_draws, .x, .y
      )
    )
    luz2_samples <- matrix(unlist(luz2_samples), nrow = n_draws, byrow = FALSE)

    z1_samples <- rnorm(n_draws, z1$z1_median, z1$z1_sd)

    # Compute klu, lw, rrs with combined uncertainty

    compute_klu <- function(luz1, luz2, z2z1) {

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

    klu <- compute_klu(luz1_samples, luz2_samples, z2z1)
    klu_estimates <- klu[[1]]
    klu_samples <- klu[[2]]

    # klu_samples <- purrr::map2(
    #   .x = klu$klu_mean,
    #   .y = klu$klu_sd,
    #   ~ rnorm(
    #     n_draws, .x, .y
    #   )
    # )
    # klu_samples <- matrix(unlist(klu_samples), nrow = n_draws, byrow = FALSE)

    compute_lw <- function(luz1, klu, z1) {

      lw_samples <- 0.54 * luz1 * exp(klu * z1)

      lw_stats <- purrr::map_dfr(
        seq_along(wavelength), ~ {
          tibble(
            lw_mean = mean(lw_samples[, .x], na.rm = T),
            lw_sd = sd(lw_samples[, .x], na.rm = T)
          )
        })

      return(list(tibble(wavelength, lw_stats), lw_samples))
    }

    lw <- compute_lw(luz1_samples, klu_samples, z1_samples)
    lw_estimates <- lw[[1]] %>%
      left_join(klu_estimates, by = "wavelength")
    lw_samples <- lw[[2]]

    # lw_samples <- purrr::map2(
    #   .x = lw$lw_mean,
    #   .y = lw$lw_sd,
    #   ~ rnorm(
    #       n_draws, .x, .y
    #   )
    # )
    # lw_samples <- matrix(unlist(lw_samples), nrow = n_draws, byrow = FALSE)

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

    rrs <- compute_rrs(lw_samples, es_samples) %>%
      left_join(lw_estimates, by = "wavelength")

    ####### Compute relative contribution of luz1

    compute_rel_unc <- function(base_sd, perturbed_sd) {
      # We compute on variance as it's additive
      return ((base_sd^2 - perturbed_sd^2) / base_sd^2)
    }

    luz1_rel_samples <- purrr::map2(
      .x = luz1$Channels_median,
      .y = luz1$Channels_sd,
      ~ rnorm(
        n_draws, .x, 0
      )
    )
    luz1_rel_samples <- matrix(unlist(luz1_rel_samples), nrow = n_draws, byrow = FALSE)

    klu_rel <- compute_klu(luz1_rel_samples, luz2_samples, z2z1)
    klu_rel_estimates <- klu_rel[[1]]
    klu_rel_samples <- klu_rel[[2]]

    lw_rel <- compute_lw(luz1_rel_samples, klu_rel_samples, z1_samples)
    lw_rel_estimates <- lw_rel[[1]]
    lw_rel_samples <- lw_rel[[2]]

    rrs_rel_estimates <- compute_rrs(lw_rel_samples, es_samples)

    rrs <- rrs %>%
      mutate(
        klu_luz1_rel_unc = compute_rel_unc(klu_sd, klu_rel_estimates$klu_sd),
        lw_luz1_rel_unc = compute_rel_unc(lw_sd, lw_rel_estimates$lw_sd),
        rrs_luz1_rel_unc = compute_rel_unc(rrs_sd, rrs_rel_estimates$rrs_sd)
      )

    ###### Compute relative contribution of luz2

    luz2_rel_samples<- purrr::map2(
      .x = luz2$Channels_median,
      .y = luz2$Channels_sd,
      ~ rnorm(
        n_draws, .x, 0
      )
    )
    luz2_rel_samples <- matrix(unlist(luz2_rel_samples), nrow = n_draws, byrow = FALSE)

    klu_rel <- compute_klu(luz1_samples, luz2_rel_samples, z2z1)
    klu_rel_estimates <- klu_rel[[1]]
    klu_rel_samples <- klu_rel[[2]]

    lw_rel <- compute_lw(luz1_samples, klu_rel_samples, z1_samples)
    lw_rel_estimates <- lw_rel[[1]]
    lw_rel_samples <- lw_rel[[2]]

    rrs_rel_estimates <- compute_rrs(lw_rel_samples, es_samples)

    rrs <- rrs %>%
      mutate(
        klu_luz2_rel_unc = compute_rel_unc(klu_sd, klu_rel_estimates$klu_sd),
        lw_luz2_rel_unc = compute_rel_unc(lw_sd, lw_rel_estimates$lw_sd),
        rrs_luz2_rel_unc = compute_rel_unc(rrs_sd, rrs_rel_estimates$rrs_sd)
      )

    ####### Compute relative contribution of z1

    lw_rel <- compute_lw(luz1_samples, klu_samples, z1$z1_median)
    lw_rel_estimates <- lw_rel[[1]]
    lw_rel_samples <- lw_rel[[2]]

    rrs_rel_estimates <- compute_rrs(lw_rel_samples, es_samples)

    rrs <- rrs %>%
      mutate(
        lw_z1_rel_unc = compute_rel_unc(lw_sd, lw_rel_estimates$lw_sd),
        rrs_z1_rel_unc = compute_rel_unc(rrs_sd, rrs_rel_estimates$rrs_sd)
      )

    ###### Compute relative contribution of es

    es_rel_samples <- purrr::map2(
      .x = es$Channels_median,
      .y = es$Channels_sd,
      ~ rnorm(
        n_draws, .x, 0
      )
    )
    es_rel_samples <- matrix(unlist(es_rel_samples), nrow = n_draws, byrow = FALSE)

    rrs_rel_estimates <- compute_rrs(lw_samples, es_rel_samples)

    # Different result with Matrix * Matrix and Matrix + Vector ?
    #rrs_rel_estimates_2 <- compute_rrs(lw_samples, es$Channels_median)

    rrs <- rrs %>%
      mutate(
        rrs_es_rel_unc = compute_rel_unc(rrs_sd, rrs_rel_estimates$rrs_sd)
      )

    ####### Check unity of relative contribution

    rrs <- rrs %>%
      mutate(
        klu_rel_unity = klu_luz1_rel_unc + klu_luz2_rel_unc,
        lw_rel_unity = lw_luz1_rel_unc + lw_luz2_rel_unc + lw_z1_rel_unc,
        rrs_rel_unity = rrs_luz1_rel_unc + rrs_luz2_rel_unc + rrs_z1_rel_unc + rrs_es_rel_unc
      )

    return(rrs)
  }

  es <- df_pdf %>%
    filter(SN %in% c(1397, 1396, 0341))

  luz1 <- df_pdf %>%
    filter(SN %in% c(1415, 1413, 0237))

  luz2 <- df_pdf %>%
    filter(SN %in% c(1416, 1414, 0238))

  rrs <- propagate_uncertainty(wave_seq, es, luz1, luz2, z1, z2z1)

  L2Data <- rrs

  # L1bDataWide <- L1bData %>%
  #   mutate(CalData = purrr::map(
  #     CalData,
  #     ~ pivot_wider(
  #       .,
  #       names_from = all_of(c("Type", "Wavelength")),
  #       names_sep = "_",
  #       values_from = Channels
  #     ) %>%
  #       ungroup()
  #   )) %>%
  #   ungroup()
  #
  # L1bDataWide <- L1bDataWide %>%
  #   mutate(CalData = purrr::map(CalData, ~ filter(., QC == "1")))
  #
  # L1bDataWide <- L1bDataWide %>%
  #   mutate(CalData = purrr::map(CalData, ~ summarise(.x, across(.cols = !matches("ID|QC|DateTime"), ~ mean(.x, na.rm = T)))))
  #
  # Es <- L1bDataWide %>%
  #   filter(SN %in% "1397" | SN == "1396" | SN == "0341") %>%
  #   unnest(cols = c(CalData)) %>%
  #   select(!matches("Instrument|SN|DateTime|UUID"))
  #
  # LuZ1 <- L1bDataWide %>%
  #   filter(SN == "1415" | SN == "1413" | SN == "0237") %>%
  #   unnest(cols = c(CalData)) %>%
  #   select(!matches("Instrument|SN|DateTime|UUID"))
  #
  # LuZ2 <- L1bDataWide %>%
  #   filter(SN == "1416" | SN == "1414" | SN == "0238") %>%
  #   unnest(cols = c(CalData)) %>%
  #   select(!matches("Instrument|SN|DateTime|UUID"))
  #
  # # Z1Z2Depth <- 0.15 # Algae Wise 2022
  #
  # # Consider suppressWarnings(expr)
  #
  # KLuWide <- suppressWarnings((log(LuZ1) - log(LuZ2)) / Z1Z2Depth)
  #
  # KLuWide <- rename_with(KLuWide, ~ str_replace(.x, "LU", "KLu"))
  #
  # KLuLong <- KLuWide %>%
  #   pivot_longer(
  #     .,
  #     cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
  #     values_to = "KLu",
  #     names_to = c("Wavelength"),
  #     # names_sep = "_",
  #     names_prefix = "[[:alpha:]]{3}_",
  #     names_transform = list(Wavelength = as.numeric)
  #   )
  #
  # if (Loess) {
  #   KLuloess <- loess(
  #     KLu ~ Wavelength,
  #     data = KLuLong,
  #     na.action = "na.omit",
  #     span = Span
  #   )
  #
  #   KLuLong <- KLuLong %>%
  #     mutate(
  #       KLu_loess = predict(KLuloess, Wavelength)
  #     )
  #
  #   KLuWide <- KLuLong %>%
  #     select(Wavelength, KLu_loess) %>%
  #     pivot_wider(
  #       names_from = "Wavelength",
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
  # RrsLong <- RrsWide %>%
  #   pivot_longer(
  #     .,
  #     cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
  #     values_to = "Rrs",
  #     names_to = c("Wavelength"),
  #     # names_sep = "_",
  #     names_prefix = "[[:alpha:]]{2}_",
  #     names_transform = list(Wavelength = as.numeric)
  #   )
  #
  # QWIP <- qc_qwip(Waves = RrsLong$Wavelength, Rrs = RrsLong$Rrs)
  #
  # # RrsLong <- RrsLong %>%
  # #   mutate(
  # #     ScoreQWIP = QWIP$Score
  # #   )
  #
  # Obs$MetadataL2 <- Obs$MetadataL2 %>%
  #   mutate(
  #     ScoreQWIP = QWIP$Score
  #   )
  #
  # if (Loess) {
  #   Rrsloess <- loess(
  #     Rrs ~ Wavelength,
  #     data = RrsLong,
  #     na.action = "na.omit",
  #     span = Span
  #   )
  #
  #   RrsLong <- RrsLong %>%
  #     mutate(
  #       Rrs_loess = predict(Rrsloess, Wavelength)
  #     )
  # }
  #
  # L2Data <- left_join(RrsLong, KLuLong, by = "Wavelength")


  # RbII computation --------------------------------------------------------

  # if(!is.null(Obs$BioSonic$L2$BottomElevation_m)) {
  #
  #   EsLong <- L1bAproxLong %>%
  #     select(!AproxData) %>%
  #     filter(SN == "1397" | SN == "1396") %>%
  #     unnest(cols = c(IntData)) %>%
  #     select(!matches("Instrument|SN|DateTime|CalData|UUID|Type")) %>%
  #     rename(Es = Channels)
  #
  #   LuZ2Long <- L1bAproxLong %>%
  #     select(!AproxData) %>%
  #     filter(SN == "1416" | SN == "1414") %>%
  #     unnest(cols = c(IntData)) %>%
  #     select(!matches("Instrument|SN|DateTime|CalData|UUID|Type")) %>%
  #     rename(LuZ2 = Channels)
  #
  #   RbII <- left_join(KLuLong, EsLong, by = c("Wavelength")) %>%
  #     left_join(LuZ2Long, by = c("Wavelength"))
  #
  #   RbII <- RbII %>%
  #     mutate(
  #       Edb = Es/exp(-KLu_loess*Obs$BioSonic$L2$BottomElevation_m),
  #       Lub = LuZ2/exp(-KLu_loess*Obs$BioSonic$L2$BottomElevation_m),
  #       RbII = (pi*Lub)/Edb
  #     ) %>%
  #     select(Wavelength, RbII)
  #
  #   L2Data <- L2Data %>%
  #     left_join(RbII, by = "Wavelength")
  #
  # }


  # Populate UUID if exist
  if (any(names(L1bData) == "UUID")) {
    L2Data <- L2Data %>%
      mutate(UUID = unique(L1bData$UUID))
  }

  return(L2Data)
}
