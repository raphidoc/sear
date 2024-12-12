#' read_devices
#'
#' @description a fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

read_hb_devices <- function(DevFile, date) {
  DevTbl <- tibble(
    Raw = readr::read_lines(DevFile, skip = 1)
  )

  DevTbl <- DevTbl %>%
    # separate have been superseded
    separate(Raw, into = c("time", "Data"), sep = " ")

  NMEA <- DevTbl %>%
    separate_wider_regex(
      col = Data,
      pattern = c("[$]", Trame = "[[:alpha:]]+", ",", Data = ".*"),
      too_few = "align_start",
      cols_remove = T
    ) %>%
    mutate(
      date_time = ymd_hms(paste0(date, time)),
      date_time = round_date(date_time, unit = "second") # format(date_time, "%Y-%m-%d %H:%M:%OS0") # Take time to second
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
        "utc",
        "lat",
        "ns",
        "lon",
        "ew",
        "gps",
        "n_sat",
        "horizontal_dilution",
        "altitude",
        "altitude_unit",
        "geoidal_separation",
        "geoidal_separation_unit",
        "gps_age",
        "diff_id"
      )
    ) %>% # Filter malformated lat and lon field
    filter(str_detect(lat, "[:digit:]{4}\\.[:digit:]{5}") & str_detect(lon, "[:digit:]{5}\\.[:digit:]{5}")) %>% # Filter incorect N S W e field
    filter(ns %in% c("N", "S") & ew %in% c("e", "W"))

  # There is some wrong data from time to time (missing field, incomplete or other format lat lon),
  # Will have to elucidate that (Applanix or DataLogger issue ?)

  GGA <- RawGGA %>%
    mutate(
      lat_d = as.numeric(str_sub(lat, 1, 2)),
      lat_dm = as.numeric(str_sub(lat, 3, 10)),
      lon_d = as.numeric(str_sub(lon, 1, 3)),
      lon_dm = as.numeric(str_sub(lon, 4, 11)),
      lat_dd = lat_d + (lat_dm / 60),
      lon_dd = lon_d + (lon_dm / 60),
      altitude = as.numeric(altitude)
    ) %>%
    mutate(
      lat_dd = ifelse(ns == "N", lat_dd, -lat_dd),
      lon_dd = ifelse(ew == "W", -lon_dd, lon_dd),
      lat = lat_dd,
      lon = lon_dd
    )

  GGA <- GGA %>%
    select(time, date_time, lat, lon, horizontal_dilution, altitude, altitude_unit)

  GGA <- unique_date_time_second(GGA)

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
      select(!time) # Avoid duplication in join

    DBT <- RawDBT %>%
      mutate(
        DBT_feet = as.numeric(DBT_feet),
        DBT_meter = as.numeric(DBT_meter),
        DBT_fathom = as.numeric(DBT_fathom)
      )

    DBT <- unique_date_time_second(DBT)
  } else {
    DBT <- tibble(
      date_time = NA,
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
          "heading", # True vessel heading 0 to 359.99: xxx.xx [degrees]
          "headingStatus", # L: low alarm, M: low warning, N: normal, O: high warning, P: high alarm, c: Tuning analog circuit
          "pitch", # pitch
          "pitchStatus", # Same as heading status
          "roll", # roll
          "rollStatus", # Same as heading status
          "Checksum" # NA: *hh
        )
      ) %>%
      select(!time) # Avoid duplication in join

    PTNTHPR <- RawPTNTHPR %>%
      mutate(
        heading = as.numeric(heading),
        pitch = as.numeric(pitch),
        roll = as.numeric(roll)
      )

    PTNTHPR <- unique_date_time_second(PTNTHPR)
  } else {
    PTNTHPR <- tibble(
      date_time = NA,
      heading = NA,
      headingStatus = NA,
      pitch = NA,
      pitchStatus = NA,
      roll = NA,
      rollStatus = NA,
      Checksum = NA
    )
  }

  # Join with date_time at the second
  MainLog <- left_join(GGA, PTNTHPR, by = c("date_time"))
  MainLog <- left_join(MainLog, DBT, by = c("date_time"))

  MainLog <- MainLog %>% rename(date = date_time, lat = lat, lon = lon)

  # Solar altitude above the horizon in radian and azimuth in radian from south to west
  PosSol <- suncalc::getSunlightPosition(data = MainLog, keep = c("altitude", "azimuth"))

  MainLog <- left_join(MainLog, PosSol, by = c("date", "lat", "lon")) %>%
    rename(date_time = date, lat = lat, lon = lon, sol_azi = azimuth, sol_zen = altitude) %>%
    mutate(
      sol_azi = sol_azi * 180 / pi + 180, # convert rad to degree and shift to north reference
      sol_zen = sol_zen * 180 / pi,
      boat_raa = sol_azi - heading,
      boat_raa = if_else(boat_raa < 0, boat_raa + 360, boat_raa)
    )

  # Add speed_kmh

  MainLog <- MainLog %>%
    mutate(
      timeDiff = date_time - lag(date_time)
    )

  lat_min <- MainLog$lat[seq(1, length(MainLog$lat) - 1, by = 2)]
  lon_min <- MainLog$lon[seq(1, length(MainLog$lat) - 1, by = 2)]
  lat_max <- MainLog$lat[seq(2, length(MainLog$lat), by = 2)]
  lon_max <- MainLog$lon[seq(2, length(MainLog$lat), by = 2)]
  timeDiff <- MainLog$timeDiff[seq(2, length(MainLog$lat), by = 2)] # in seconds

  speed <- purrr::pmap(
    list(lat_min, lon_min, lat_max, lon_max, timeDiff),
    ~ pracma::haversine(c(..1, ..2), c(..3, ..4)) / ..5 * 3600
  ) # in kmh

  SpeedApprox <- approx(
    x = MainLog$date_time[seq(2, length(MainLog$lat), by = 2)],
    y = speed,
    xout = MainLog$date_time
  )[[2]]

  MainLog <- MainLog %>%
    mutate(
      speed_kmh = SpeedApprox
    )

  return(MainLog)
}
