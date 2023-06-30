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

  MTELog %>%
    # separate have been superseded
    separate(Raw, into = c("Time", "Instrument", "Data"), sep = c(12, 19)) %>%
    mutate(
      DateTime = lubridate::ymd_hms(paste(Date, Time)),
      Instrument = str_extract(Instrument, "[:alnum:]+")
    )
}

# Apla extractor ----------------------------------------------------------

read_apla <- function(MTELog) {
  Apla <- MTELog %>%
    filter(Instrument == "APLA") %>%
    separate(
      col = Data,
      sep = ",",
      into = c("Trame", "Data"),
      extra = "merge"
    ) %>%
    mutate(
      Trame = str_extract(Trame, "[[:alpha:]]+"),
      Time = str_remove(Time, ".{4}$") # Take Time to second
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

  # There is some wrong data from time to time (missing field, incomplete or other format Lat Lon), Will have to elucidate that (Applanix or DataLogger issue ?)

  # Could add a filter based on the pattern [:digit:]{4}\.[:digit:]{5} for Lat and [:digit:]{5}\.[:digit:]{5} for Lon

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

  # Add default NA value to ObsType for initial map plot
  GGA <- GGA %>%
    select(Time, DateTime, Lat_DD, Lon_DD, HorizontalDilution, Altitude, AltitudeUnit)

  # Extract VTG info, course and speed

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
        "Course_TN", # Degrees
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
    select(!DateTime) # Avoid duplication in join

  VTG <- RawVTG %>%
    mutate(
      Course_TN = as.numeric(Course_TN),
      Course_MN = as.numeric(Course_MN),
      Speed_N = as.numeric(Speed_N),
      Speed_kmh = as.numeric(Speed_kmh)
    )

  # Join with Time at the second
  Apla <- left_join(GGA, VTG, by = c("Time"))

  Apla <- Apla %>% rename(date = DateTime, lat = Lat_DD, lon = Lon_DD)

  # Solar altitude above the horizon in radian and azimuth in radian from south to west
  PosSol <- suncalc::getSunlightPosition(data = Apla, keep = c("altitude", "azimuth"))

  Apla <- left_join(Apla, PosSol, by = c("date", "lat", "lon")) %>%
    rename(DateTime = date, Lat_DD = lat, Lon_DD = lon, SolAzm = azimuth, SolAlt = altitude) %>%
    mutate(
      SolAzm = SolAzm * 180 / pi + 180, # convert rad to degree and shift to north reference
      SolAlt = SolAlt * 180 / pi,
      BoatSolAzm = SolAzm - Course_TN,
      BoatSolAzm = if_else(BoatSolAzm < 0, BoatSolAzm + 360, BoatSolAzm)
    )

  Apla <- Apla %>% filter(Speed_N <= 10)
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
