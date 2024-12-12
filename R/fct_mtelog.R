#' Read and tidy data from Applanix, CTD, ECO, SeaOWL, SUNA
#'
#' @description
#' The read function take as input a first parsing of the txt file of
#' the data logger
#'
#' @import stringr
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import readr


# Read log ----------------------------------------------------------------

read_mtelog <- function(LogFile) {
  MTELog <- tibble(
    Raw = readr::read_lines(LogFile)
  )

  # Exctract date on line 3 of datalogger header
  date <- str_extract(MTELog[3, ], "[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}")

  MTELog <- MTELog[7:length(rownames(MTELog)), ]

  # With file DATA_20230720_164631.txt the following error occur:
  # Error in nchar: invalid multibyte string, element 336
  # MTELog[336,]

  # The error also occur in files: DATA_20230921_202037.txt, DATA_20230921_202053.txt
  # It occur at the precise same time which indicate that it originate from the custom GNSS

  MTELog %>%
    # separate have been superseded
    separate(Raw, into = c("time", "instrument", "Data"), sep = c(12, 19)) %>%
    mutate(
      date_time = lubridate::ymd_hms(paste(date, time)),
      instrument = str_extract(instrument, "[:alnum:]+")
    )
}

# NMEA parsing ------------------------------------------------------------

read_apla <- function(MTELog) {
  # TODO Add NMEA checksum verification

  Apla <- MTELog %>%
    filter(instrument == "APLA") %>%
    separate_wider_regex(
      col = Data,
      pattern = c("\\s[$]", Trame = "[[:alpha:]]+", ",", Data = ".*"),
      too_few = "align_start",
      cols_remove = T
      # into = c("Trame", "Data"),
      # extra = "merge"
    ) %>%
    mutate(
      Trame = str_extract(Trame, "[[:alpha:]]+"),
      # time = str_remove(time, ".{4}$"),
      date_time = round_date(date_time, unit = "second") # format(date_time, "%Y-%m-%d %H:%M:%OS0") # Take time to second
    ) %>%
    filter(
      !is.na(Trame)
    )

  RawGGA <- Apla %>%
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
    filter(ns %in% c("N", "S") & ew %in% c("E", "W"))

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
      lon_dd = ifelse(ew == "W", -lon_dd, lon_dd)
    ) %>%
    mutate(lon = lon_dd, lat = lat_dd)

  GGA <- GGA %>%
    select(time, date_time, lat, lon, horizontal_dilution, altitude, altitude_unit)

  GGA <- unique_date_time_second(GGA)

  # Extract VTG info, course and speed

  if (any(str_detect(Apla$Trame, "[:alpha:]{2}VTG"))) {
    RawVTG <- Apla %>%
      filter(str_detect(Trame, "[:alpha:]{2}VTG")) %>%
      pivot_wider(
        names_from = Trame,
        values_from = Data
      ) %>%
      separate(
        col = contains("VTG"),
        sep = ",",
        convert = T,
        into = c(
          "course_tn", # Degrees Applanix doc state: True vessel track in the vessel frame (Actual course and speed relative to the ground).
          "reference_tn", # True north
          "course_mn", # Degrees
          "reference_mn", # Magnetic
          "speed_kn", # Measured horizontal speed (in knots)
          "unit_kn", # Knots (N)
          "speed_kmh", # Measured horizontal speed (in km/h)
          "unit_kmh", # km/h
          "mode_checksum" # a = Autonomous, D = DGPS, e = DR
        )
      ) %>%
      select(!time) # Avoid duplication in join

    VTG <- RawVTG %>%
      mutate(
        course_tn = as.numeric(course_tn),
        course_mn = as.numeric(course_mn),
        speed_kn = as.numeric(speed_kn),
        speed_kmh = as.numeric(speed_kmh)
      )

    VTG <- unique_date_time_second(VTG)
  } else {
    VTG <- tibble(
      date_time = NA,
      course_tn = NA,
      reference_tn = NA,
      course_mn = NA,
      reference_mn = NA,
      speed_kn = NA,
      unit_kn = NA,
      speed_kmh = NA,
      unit_kmh = NA,
      mode_checksum = NA
    )
  }

  # Extract pitch and roll from Applanix PASHR data

  if (any(str_detect(Apla$Trame, "PASHR"))) {
    RawPASHR <- Apla %>%
      filter(str_detect(Trame, "PASHR")) %>%
      pivot_wider(
        names_from = Trame,
        values_from = Data
      ) %>%
      separate(
        col = contains("PASHR"),
        sep = ",",
        convert = T,
        into = c(
          "utc", # utc time: hhmmss.sss
          "heading", # True vessel heading 0 to 359.99: xxx.xx [degrees]
          "true", # True
          "roll", # roll -90.00 to +90.00: RRR.RR [degrees]
          "pitch", # pitch -90.00 to +90.00: PPP.PP [degrees]
          "heave", # heave -99.00 to +99.00: HHH.HH [meters]
          "roll_accuracy", # 0 to 9.999: a.aaa [degrees]
          "pitch_accuracy", # 0 to 9.999: b.bbb [degrees]
          "heading_accuracy", # 0 to 9.999: c.ccc [degrees]
          "heading_Flag", # 0 = no aiding, 1 = GNSS aiding, 2 = GNSS & GAMS aiding
          # "IMU_Flag", # 0 = IMU out, 1 = satisfactory *DOSENT SEEM TO BE IN DATA*
          "Checksum" # NA: *hh
        )
      ) %>%
      select(!time) # Avoid duplication in join

    PASHR <- RawPASHR %>%
      mutate(
        heading = as.numeric(heading),
        roll = as.numeric(roll),
        pitch = as.numeric(pitch),
        heave = as.numeric(heave),
        roll_accuracy = as.numeric(roll_accuracy),
        pitch_accuracy = as.numeric(pitch_accuracy),
        heading_accuracy = as.numeric(heading_accuracy),
        heading_Flag = as.numeric(heading_Flag)
      )

    PASHR <- unique_date_time_second(PASHR)
  } else {
    PASHR <- tibble(
      date_time = NA,
      utc = NA,
      heading = NA,
      true = NA,
      roll = NA,
      pitch = NA,
      heave = NA,
      roll_accuracy = NA,
      pitch_accuracy = NA,
      heading_accuracy = NA,
      heading_Flag = NA,
      Checksum = NA
    )
  }

  # Join with date_time at the second
  Apla <- left_join(GGA, VTG, by = c("date_time"))
  Apla <- left_join(Apla, PASHR, by = c("date_time"))

  Apla <- Apla %>% rename(date = date_time, lat = lat, lon = lon)

  # Solar altitude above the horizon in radian and azimuth in radian from south to west
  PosSol <- suncalc::getSunlightPosition(
    data = Apla,
    keep = c("altitude", "azimuth")
    ) %>%
    rename(
      sol_altitude = altitude,
      sol_azimuth = azimuth
    )

  Apla <- left_join(Apla, PosSol, by = c("date", "lat", "lon")) %>%
    rename(date_time = date, lat = lat, lon = lon) %>%
    mutate(
      sol_azi = (sol_azimuth * 180 / pi) + 180, # convert rad to degree and shift to north reference
      sol_zen = 90 - (sol_altitude * 180 / pi), # shift altitude above the horizon in rad to zenith angle
      boat_raa = sol_azi - course_tn,
      boat_raa = if_else(boat_raa < 0, boat_raa + 360, boat_raa)
    )

  #Apla <- Apla %>% filter(speed_kmh <= 15)

  return(Apla)
}

# BBFL2 extractor -----------------------------------------------------------

read_bbfl2 <- function(MTELog) {
  BBFL2 <- MTELog %>%
    filter(instrument == "ECO") %>%
    separate(
      col = Data,
      sep = "(?!^)\\s",
      into = c(
        "bbfl2_date",
        "bbfl2_time",
        "ntu_ref", # ntu
        "ntu_channel",
        "pe_ref", # phycoerithrin
        "pe_channel",
        "pc_ref", # phycocyanine
        "pc_channel",
        "g"
      ),
      convert = T,
      extra = "merge"
    ) %>%
    mutate(
      # Trame = str_extract(Trame, "[[:alpha:]]+"),
      time = str_remove(time, ".{4}$") # Take time to second
    ) %>%
    drop_na()
}

# SeaOWL extractor -----------------------------------------------------------

read_seaowl <- function(MTELog) {
  SeaOWL <- MTELog %>%
    filter(instrument == "OWL") %>%
    separate(
      col = Data,
      sep = "(?!^)\\s",
      into = c(
        "sn",
        "chl_led_forward_voltage",
        "chl_low_gain_raw_count",
        "chl_high_gain_raw_count",
        "chl_channel",
        "vsf_700_led_forward_voltage",
        "vsf_700_low_gain_raw_count",
        "vsf_700_high_gain_raw_count",
        "vsf_700_channel",
        "fdom_led1_forward_voltage",
        "fdom_led2_forward_voltage",
        "fdom_low_gain_raw_count",
        "fdom_high_gain_raw_count",
        "fdom_output"
      ),
      convert = T,
      extra = "merge"
    ) %>%
    mutate(
      # Trame = str_extract(Trame, "[[:alpha:]]+"),
      time = str_remove(time, ".{4}$") # Take time to second
    ) %>%
    drop_na()
}

# SBE19 extractor -----------------------------------------------------------

read_sbe19 <- function(MTELog) {
  SBE19 <- MTELog %>%
    filter(instrument == "CTD") %>%
    separate(
      col = Data,
      sep = ",\\s",
      into = c(
        "temperature", # Celsius ITS-90
        "conductivity", # S/m, multiply by 10 to get mS/cm
        "pressure", # decibars
        "volt0", # volt0 ? = OXY
        "Volt2" # Volt2 ? = ph
      ),
      convert = T,
      extra = "merge"
    ) %>%
    mutate(
      # Trame = str_extract(Trame, "[[:alpha:]]+"),
      time = str_remove(time, ".{4}$") # Take time to second
    ) %>%
    drop_na()
}
