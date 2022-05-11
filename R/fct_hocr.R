#' hocr
#' Reader for HOCR binary file saved by datalogger
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


read_hocr <- function(BinFile){

  # now source python onload
  reticulate::source_python(system.file("py","hocr.py", package = "sear", mustWork = T))

  RawHOCR <- purrr::map(BinFile, Hocr$from_file)

  RawHOCR <- unlist(purrr::map(RawHOCR , ~ .x$packets))

  # check for invalid packet
  ValidInd <- purrr::map_lgl(RawHOCR, ~ str_detect(as.character(.x$instrument, errors="ignore"), "SAT(HPL|HSE|HED|PLD)"))

  if (any(!ValidInd)){

    warning("Invalid HOCR packets detected and removed: ", length(which(!ValidInd)))

    RawHOCR[ValidInd]

  } else {
    RawHOCR
  }
}

filter_hocr <- function(RawHOCR, TimeIndexHOCR, TimeInt) {

  # Ideally the packet DateTime would be construct from the packet only ...
  # As I don't know the Date here quick and dirty fix with AplaDate

  ind <- purrr::map_lgl(.x = TimeIndexHOCR, ~ .x %within% TimeInt)

  RawHOCR[ind]
}

tidy_hocr <- function(Packets, AplaDate){
  tibble::tibble(
    # Applanix time added by the DataLogger in millisecond
    # Unkown Date format in the binary file, so take the one in the txt file
    GPSTime = as.POSIXct(paste0(AplaDate, hms::as_hms(Packets$gpstime/1000)), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),

    # HOCR Packets
    # Fix missing byte bug by ignoring decoding error
    Instrument = as.character(Packets$instrument, errors="ignore"),
    SN = as.character(Packets$sn, errors="ignore"),
    IntTime = Packets$inttime,
    SampleDelay = Packets$sampledelay,
    DarkSample = Packets$darksample,
    DarkAverage = Packets$darkaverage,
    SpecTemp = as.character(Packets$spectemp, errors="ignore"),
    Frame = Packets$frame,
    Timer = as.character(Packets$timer, errors="ignore"),
    CheckSum = Packets$checksum,
    Channels = Packets$channel
  )
}

cal_hocr <- function(FiltRawHOCR, CalHOCR, AplaDate){

  RawData <- purrr::map_df(FiltRawHOCR, ~ tidy_hocr(., AplaDate))

  # Bind HOCR with Calibration by Instrument (shutter mode) -----------------

  # There will be a problem here if some HCOR packet have corrupted Instrument and SN.
  # Have to filter and remove those corrupted pakect before, in the tidy_hocr function ?

  GlobCal <- RawData %>% group_by(Instrument, SN) %>% nest(RawData = !matches("Instrument|SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$OPTIC3, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$THERM1, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$INTTIME, by = c("Instrument", "SN"))

  GlobCal <- left_join(GlobCal, CalHOCR$SAMPLE, by = c("Instrument", "SN"))


  # Add OPTIC3 to raw data --------------------------------------------------

  # taken from: https://gist.github.com/mdlincoln/528a53939538b07ade86
  row_rep <- function(df, n) {
    df[rep(1:nrow(df), times = n),]
  }

  GlobCal <- GlobCal %>% mutate(RawData = purrr::map2(RawData, OPTIC3, ~ bind_cols(.x, row_rep(.y, nrow(.x)/nrow(.y)))))

  # Calibrate time ----------------------------------------------------------

  cal_inttime <- function(RawData, INTTIME){
    a0 <- INTTIME$a0
    a1 <- INTTIME$a1

    purrr::modify_in(RawData, .where = "IntTime", ~(a0*.x^0) + (a1*.x^1))
  }

  GlobCal <- GlobCal %>% mutate(CalData = purrr::map2(.x = RawData, .y = INTTIME, .f = ~cal_inttime(.x, .y)))


  # Calibrate optical channels ----------------------------------------------

  cal_optic3 <- function(.x, Instrument){

    if (str_detect(Instrument, "HSE|HED")) { # In air

      dplyr::mutate(.data = .x, Channels = 1.0*a1*(Channels-a0)*(cint/IntTime))

    } else if (str_detect(Instrument, "HPL|PLD")){ # In water

      dplyr::mutate(.data = .x, Channels = im*a1*(Channels-a0)*(cint/IntTime))

    } else {
      warning(paste0("Instrument name not valid: ", Instrument))
    }
  }

  GlobCal <- GlobCal %>% mutate(CalData = purrr::map2(.x = CalData, .y = Instrument, ~ cal_optic3(.x, .y)))


  # Interpolate time coordinate ---------------------------------------------

  HOCRLong <- GlobCal %>% # OPTIC3
    mutate(CalData = purrr::map(
      CalData,
      ~ select(.x, !all_of(c("Units","FieldLength","DataType","CalLines","FitType","a0","a1","im","cint")))
    )) %>% # Packet metadata
    mutate(CalData = purrr::map(
      CalData, ~ select(.x, !all_of(c("SampleDelay","DarkSample","DarkAverage","SpecTemp","Frame","Timer","CheckSum")))
    )) %>%
    select(Instrument, SN, CalData) %>%
    filter(str_detect(Instrument, "HSE|HPL"))

  # Convert to wide format

  HOCRWide <- HOCRLong %>%
    mutate(CalData = purrr::map(
      CalData,
      ~ pivot_wider(
        .,
        names_from = all_of(c("Type", "Wavelength")),
        names_sep = "_",
        values_from = Channels
      ))) %>%
    mutate(CalData = purrr::map(CalData, ~ select(., where(function(x) all(!is.na(x))))))

  # Compute the Time Sequence

  ShortNobs <- HOCRWide %>%
    mutate(Nobs = purrr::map_dbl(CalData, ~ length(rownames(.))))

  ShortNobs <- ShortNobs %>%
    filter(Nobs == min(ShortNobs$Nobs)) %>%
    unnest(cols = c(CalData))

  MinTime <- min(ShortNobs$GPSTime)
  #format(MinTime, "%Y-%m-%d %H:%M:%OS3")

  MaxTime <- max(ShortNobs$GPSTime)
  #format(MaxTime, "%Y-%m-%d %H:%M:%OS3")

  TimeSeq <- seq.POSIXt(MinTime, MaxTime, by = min(ShortNobs$IntTime))
  #format(TimeSeq, "%Y-%m-%d %H:%M:%OS3")

  # Interpolate to commom time coordinates

  HOCRWide <- HOCRWide %>%
    mutate(CalData = purrr::map(CalData, ~ select(., !IntTime)))

  approx_tbl <- function(., TimeSeq) {

    tbl <- tibble(DateTime = TimeSeq)

    for (i in seq_along(colnames(.))[-1]) {

      coord <- approx(x = .[[1]], y = .[[i]], xout = TimeSeq, method = "linear")

      tbl <- bind_cols(tbl, x = coord[[2]])
      colnames(tbl)[i] <- colnames(.)[i]
    }

    tbl %>% mutate(ID = seq_along(TimeSeq))
  }

  HOCRWide <- HOCRWide %>%
    mutate(AproxData = purrr::map(CalData, ~ approx_tbl(., TimeSeq)))

  # Transform back to long format

  HOCRLong <- HOCRWide %>%
    mutate(AproxData = purrr::map(
      AproxData,
      ~pivot_longer(
        .,
        cols = matches("[[:alpha:]]{2}_[[:digit:]]{3}(.[[:digit:]]{1,2})?"),
        values_to = "Channels",
        names_to = c("Type", "Wavelength"),
        names_sep = "_",
        #names_prefix = "[[:alpha:]]{2}_",
        names_transform = list(Wavelength = as.numeric)
      ) %>%
        group_by(ID)
    ))

  HOCRLong

}

process_station <- function(Apla = NULL, FiltRawHOCR = NULL, CalData = NULL){
  CalHocr <- cal_hocr(FiltRawHOCR, CalData$HOCR, AplaDate = unique(date(Apla$DateTime)))
}
