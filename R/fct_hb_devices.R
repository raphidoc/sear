#' read_devices
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

read_hb_devices <- function(DevFile, Date) {
  DevTbl <- tibble(
    Raw = readr::read_lines(DevFile, skip = 1)
  )

  DevTbl <- DevTbl %>%
    # separate have been superseded
    separate(Raw, into = c("Time", "Data"), sep = " ")

  NMEA <- DevTbl %>%
    separate_wider_regex(
      col = Data,
      pattern = c("[$]", Trame = "[[:alpha:]]+", ",", Data = ".*"),
      too_few = "align_start",
      cols_remove = T
    ) %>%
    mutate(
      DateTime = ymd_hms(paste0(Date, Time)),
      DateTime = round_date(DateTime, unit = "second") # format(DateTime, "%Y-%m-%d %H:%M:%OS0") # Take Time to second
    )

  if (all(is.na(NMEA$Data))) {
    stop(paste("No NMEA data in file:", basename(DevFile)))
  }

  RawGGA <- NMEA %>%
    filter(str_detect(Trame, "[:alpha:]{2}GGA")) %>%
    pivot_wider(
      names_from = Trame,
      values_from = Data
    ) %>%
    separate(
      col = contains("GGA"),
      sep = ",",
      into = c(
        "UTC",
        "Lat",
        "NS",
        "Lon",
        "EW",
        "GPS",
        "Nsat",
        "HorizontalDilution",
        "Altitude",
        "AltitudeUnit",
        "GeoidalSeparation",
        "GeoidalSeparationUnit",
        "GPSAge",
        "DiffID"
      )
    ) %>% # Filter malformated Lat and Lon field
    filter(str_detect(Lat, "[:digit:]{4}\\.[:digit:]{5}") & str_detect(Lon, "[:digit:]{5}\\.[:digit:]{5}")) %>% # Filter incorect N S W E field
    filter(NS %in% c("N", "S") & EW %in% c("E", "W"))

  # There is some wrong data from time to time (missing field, incomplete or other format Lat Lon),
  # Will have to elucidate that (Applanix or DataLogger issue ?)

  GGA <- RawGGA %>%
    mutate(
      Lat_D = as.numeric(str_sub(Lat, 1, 2)),
      Lat_DM = as.numeric(str_sub(Lat, 3, 10)),
      Lon_D = as.numeric(str_sub(Lon, 1, 3)),
      Lon_DM = as.numeric(str_sub(Lon, 4, 11)),
      Lat_DD = Lat_D + (Lat_DM / 60),
      Lon_DD = Lon_D + (Lon_DM / 60),
      Altitude = as.numeric(Altitude)
    ) %>%
    mutate(
      Lat_DD = ifelse(NS == "N", Lat_DD, -Lat_DD),
      Lon_DD = ifelse(EW == "W", -Lon_DD, Lon_DD),
      Lat = Lat_DD,
      Lon = Lon_DD
    )

  GGA <- GGA %>%
    select(Time, DateTime, Lat, Lon, HorizontalDilution, Altitude, AltitudeUnit)

  GGA <- unique_datetime_second(GGA)

  # Extract DBT (Depth Below Transducer) info

  if (any(str_detect(NMEA$Trame, "[:alpha:]{2}DBT"))) {
    RawDBT <- NMEA %>%
      filter(str_detect(Trame, "[:alpha:]{2}DBT")) %>%
      pivot_wider(
        names_from = Trame,
        values_from = Data
      ) %>%
      separate(
        col = contains("DBT"),
        sep = ",|[*]",
        convert = F,
        into = c(
          "DBT_feet", # DBT feet
          "Feet", # feet
          "DBT_meter", # DBT meter
          "Metre", # Magnetic
          "DBT_fathom", # DBT fathom
          "Fathom", # fathom
          "Checksum"
        )
      ) %>%
      select(!Time) # Avoid duplication in join

    DBT <- RawDBT %>%
      mutate(
        DBT_feet = as.numeric(DBT_feet),
        DBT_meter = as.numeric(DBT_meter),
        DBT_fathom = as.numeric(DBT_fathom)
      )

    DBT <- unique_datetime_second(DBT)
  } else {
    DBT <- tibble(
      DateTime = NA,
      DBT_feet = NA,
      Feet = "f",
      DBT_meter = NA,
      Meter = "M",
      DBT_fathom = NA,
      Fathom = "F",
      Checksum = NA
    )
  }

  # Extract PTNTHPR heading, pitch, roll

  if (any(str_detect(NMEA$Trame, "PTNTHPR"))) {
    RawPTNTHPR <- NMEA %>%
      filter(str_detect(Trame, "PTNTHPR")) %>%
      pivot_wider(
        names_from = Trame,
        values_from = Data
      ) %>%
      separate(
        col = contains("PTNTHPR"),
        sep = ",|[*]",
        convert = T,
        into = c(
          "Heading", # True vessel heading 0 to 359.99: xxx.xx [degrees]
          "HeadingStatus", # L: low alarm, M: low warning, N: normal, O: high warning, P: high alarm, C: Tuning analog circuit
          "Pitch", # Pitch
          "PitchStatus", # Same as heading status
          "Roll", # Roll
          "RollStatus", # Same as heading status
          "Checksum" # NA: *hh
        )
      ) %>%
      select(!Time) # Avoid duplication in join

    PTNTHPR <- RawPTNTHPR %>%
      mutate(
        Heading = as.numeric(Heading),
        Pitch = as.numeric(Pitch),
        Roll = as.numeric(Roll)
      )

    PTNTHPR <- unique_datetime_second(PTNTHPR)
  } else {
    PTNTHPR <- tibble(
      DateTime = NA,
      Heading = NA,
      HeadingStatus = NA,
      Pitch = NA,
      PitchStatus = NA,
      Roll = NA,
      RollStatus = NA,
      Checksum = NA
    )
  }

  # Join with DateTime at the second
  MainLog <- left_join(GGA, PTNTHPR, by = c("DateTime"))
  MainLog <- left_join(MainLog, DBT, by = c("DateTime"))

  MainLog <- MainLog %>% rename(date = DateTime, lat = Lat, lon = Lon)

  # Solar altitude above the horizon in radian and azimuth in radian from south to west
  PosSol <- suncalc::getSunlightPosition(data = MainLog, keep = c("altitude", "azimuth"))

  MainLog <- left_join(MainLog, PosSol, by = c("date", "lat", "lon")) %>%
    rename(DateTime = date, Lat = lat, Lon = lon, SolAzm = azimuth, SolZen = altitude) %>%
    mutate(
      SolAzm = SolAzm * 180 / pi + 180, # convert rad to degree and shift to north reference
      SolZen = SolZen * 180 / pi,
      BoatSolAzm = SolAzm - Heading,
      BoatSolAzm = if_else(BoatSolAzm < 0, BoatSolAzm + 360, BoatSolAzm)
    )

  # Add Speed_kmh

  MainLog <- MainLog %>%
    mutate(
      TimeDiff = DateTime - lag(DateTime)
    )

  LatMin <- MainLog$Lat[seq(1, length(MainLog$Lat) - 1, by = 2)]
  LonMin <- MainLog$Lon[seq(1, length(MainLog$Lat) - 1, by = 2)]
  LatMax <- MainLog$Lat[seq(2, length(MainLog$Lat), by = 2)]
  LonMax <- MainLog$Lon[seq(2, length(MainLog$Lat), by = 2)]
  TimeDiff <- MainLog$TimeDiff[seq(2, length(MainLog$Lat), by = 2)] # in seconds

  Speed <- purrr::pmap(
    list(LatMin, LonMin, LatMax, LonMax, TimeDiff),
    ~ pracma::haversine(c(..1, ..2), c(..3, ..4)) / ..5 * 3600
  ) # in kmh

  SpeedApprox <- approx(
    x = MainLog$DateTime[seq(2, length(MainLog$Lat), by = 2)],
    y = Speed,
    xout = MainLog$DateTime
  )[[2]]

  MainLog <- MainLog %>%
    mutate(
      Speed_kmh = SpeedApprox
    )

  return(MainLog)
}
