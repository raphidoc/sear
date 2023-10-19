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
  Date <- str_extract(MTELog[3, ], "[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}")

  MTELog <- MTELog[7:length(rownames(MTELog)),]

  # With file DATA_20230720_164631.txt the following error occur:
  # Error in nchar: invalid multibyte string, element 336
  # MTELog[336,]

  # The error also occur in files: DATA_20230921_202037.txt, DATA_20230921_202053.txt
  # It occur at the precise same time which indicate that it originate from the custom GNSS

  MTELog %>%
    # separate have been superseded
    separate(Raw, into = c("Time", "Instrument", "Data"), sep = c(12, 19)) %>%
    mutate(
      DateTime = lubridate::ymd_hms(paste(Date, Time)),
      Instrument = str_extract(Instrument, "[:alnum:]+")
    )

}

# NMEA parsing ------------------------------------------------------------

read_apla <- function(MTELog) {

  Apla <- MTELog %>%
    filter(Instrument == "APLA") %>%
    separate_wider_regex(
      col = Data,
      pattern = c("\\s[$]", Trame = "[[:alpha:]]+", ",", Data = ".*"),
      too_few = "align_start",
      cols_remove = T
      #into = c("Trame", "Data"),
      #extra = "merge"
    ) %>%
    mutate(
      Trame = str_extract(Trame, "[[:alpha:]]+"),
      #Time = str_remove(Time, ".{4}$"),
      DateTime = round_date(DateTime, unit = "second")#format(DateTime, "%Y-%m-%d %H:%M:%OS0") # Take Time to second
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
      Lon_DD = ifelse(EW == "W", -Lon_DD, Lon_DD)
    )

  GGA <- GGA %>%
    select(Time, DateTime, Lat_DD, Lon_DD, HorizontalDilution, Altitude, AltitudeUnit)

  GGA <- unique_datetime_second(GGA)

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
          "Course_TN", # Degrees Applanix doc state: True vessel track in the vessel frame (Actual course and speed relative to the ground).
          "Reference_TN", # True north
          "Course_MN", # Degrees
          "Reference_MN", # Magnetic
          "Speed_N", # Measured horizontal speed (in knots)
          "Unit_N", # Knots (N)
          "Speed_kmh", # Measured horizontal speed (in km/h)
          "Unit_kmh", # km/h
          "Mode_Checksum" # A = Autonomous, D = DGPS, E = DR
        )
      ) %>%
      select(!Time) # Avoid duplication in join

    VTG <- RawVTG %>%
      mutate(
        Course_TN = as.numeric(Course_TN),
        Course_MN = as.numeric(Course_MN),
        Speed_N = as.numeric(Speed_N),
        Speed_kmh = as.numeric(Speed_kmh)
      )

    VTG <- unique_datetime_second(VTG)

  } else {
    VTG <- tibble(
      Time = NA,
      Course_TN = NA,
      Reference_TN = NA,
      Course_MN = NA,
      Reference_MN = NA,
      Speed_N = NA,
      Unit_N = NA,
      Speed_kmh = NA,
      Unit_kmh = NA,
      Mode_Checksum = NA
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
          "UTC", # UTC time: hhmmss.sss
          "Heading", # True vessel heading 0 to 359.99: xxx.xx [degrees]
          "T", # True
          "Roll", # Roll -90.00 to +90.00: RRR.RR [degrees]
          "Pitch", # Pitch -90.00 to +90.00: PPP.PP [degrees]
          "Heave", # Heave -99.00 to +99.00: HHH.HH [meters]
          "Roll_Accuracy", # 0 to 9.999: a.aaa [degrees]
          "Pitch_Accuracy", # 0 to 9.999: b.bbb [degrees]
          "Heading_Accuracy", # 0 to 9.999: c.ccc [degrees]
          "Heading_Flag", # 0 = no aiding, 1 = GNSS aiding, 2 = GNSS & GAMS aiding
          #"IMU_Flag", # 0 = IMU out, 1 = satisfactory *DOSENT SEEM TO BE IN DATA*
          "Checksum" # NA: *hh
        )
      ) %>%
      select(!Time) # Avoid duplication in join

    PASHR <- RawPASHR %>%
      mutate(
        Heading = as.numeric(Heading),
        Roll = as.numeric(Roll),
        Pitch = as.numeric(Pitch),
        Heave = as.numeric(Heave),
        Roll_Accuracy = as.numeric(Roll_Accuracy),
        Pitch_Accuracy = as.numeric(Pitch_Accuracy),
        Heading_Accuracy = as.numeric(Heading_Accuracy),
        Heading_Flag = as.numeric(Heading_Flag)
      )

    PASHR <- unique_datetime_second(PASHR)

  } else {
    PASHR <- tibble(
      Time = NA,
      UTC = NA,
      Heading = NA,
      T = NA,
      Roll = NA,
      Pitch = NA,
      Heave = NA,
      Roll_Accuracy = NA,
      Pitch_Accuracy = NA,
      Heading_Accuracy = NA,
      Heading_Flag = NA,
      Checksum = NA
    )
  }

  # Join with DateTime at the second
  Apla <- left_join(GGA, VTG, by = c("DateTime"))
  Apla <- left_join(Apla, PASHR, by = c("DateTime"))

  Apla <- Apla %>% rename(date = DateTime, lat = Lat_DD, lon = Lon_DD)

  # Solar altitude above the horizon in radian and azimuth in radian from south to west
  PosSol <- suncalc::getSunlightPosition(data = Apla, keep = c("altitude", "azimuth"))

  Apla <- left_join(Apla, PosSol, by = c("date", "lat", "lon")) %>%
    rename(DateTime = date, Lat_DD = lat, Lon_DD = lon, SolAzm = azimuth, SolZen = altitude) %>%
    mutate(
      SolAzm = SolAzm * 180 / pi + 180, # convert rad to degree and shift to north reference
      SolZen = SolZen * 180 / pi,
      BoatSolAzm = SolAzm - Course_TN,
      BoatSolAzm = if_else(BoatSolAzm < 0, BoatSolAzm + 360, BoatSolAzm)
    )

  Apla <- Apla %>% filter(Speed_kmh <= 15)
}

# BBFL2 extractor -----------------------------------------------------------

read_bbfl2 <- function(MTELog) {

  BBFL2 <- MTELog %>%
    filter(Instrument == "ECO") %>%
    separate(
      col = Data,
      sep = "(?!^)\\s",
      into = c(
        "BBFL2Date",
        "BBFL2Time",
        "NTURef", # NTU
        "NTUSig",
        "PERef", # phycoerithrin
        "PESig",
        "PCRef", # phycocyanine
        "PCSig",
        "g"
      ),
      convert = T,
      extra = "merge"
    ) %>%
    mutate(
      # Trame = str_extract(Trame, "[[:alpha:]]+"),
      Time = str_remove(Time, ".{4}$") # Take Time to second
    ) %>%
    drop_na()
}

# SeaOWL extractor -----------------------------------------------------------

read_seaowl <- function(MTELog) {
  SeaOWL <- MTELog %>%
    filter(Instrument == "OWL") %>%
    separate(
      col = Data,
      sep = "(?!^)\\s",
      into = c(
        "SN",
        "ChlLEDForwardVoltage",
        "ChlLowGainRawCounts",
        "ChlHighGainRawCounts",
        "ChlOutput",
        "Bb700LEDForwardVoltage",
        "Bb700LowGainRawCounts",
        "Bb700HighGainRawCounts",
        "Bb700Output",
        "FDOMLED1ForwardVoltage",
        "FDOMLED2ForwardVoltage",
        "FDOMLowGainRawCounts",
        "FDOMHighGainRawCounts",
        "FDOMOutput"
      ),
      convert = T,
      extra = "merge"
    ) %>%
    mutate(
      # Trame = str_extract(Trame, "[[:alpha:]]+"),
      Time = str_remove(Time, ".{4}$") # Take Time to second
    ) %>%
    drop_na()
}

# SBE19 extractor -----------------------------------------------------------

read_sbe19 <- function(MTELog) {
  SBE19 <- MTELog %>%
    filter(Instrument == "CTD") %>%
    separate(
      col = Data,
      sep = ",\\s",
      into = c(
        "Temperature", # Celsius ITS-90
        "Conductivity", # S/m, multiply by 10 to get mS/cm
        "Pressure", # decibars
        "Volt0", # Volt0 ? = OXY
        "Volt2" # Volt2 ? = pH
      ),
      convert = T,
      extra = "merge"
    ) %>%
    mutate(
      # Trame = str_extract(Trame, "[[:alpha:]]+"),
      Time = str_remove(Time, ".{4}$") # Take Time to second
    ) %>%
    drop_na()
}
