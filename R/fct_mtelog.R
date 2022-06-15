#' Read and tidy data from Applanix, CTD, BB3, SeaOWL, SUNA
#'
#' @description
#' The read function take as input MainLog, a first parsing of the txt file of
#' the data logger
#'
#' @import stringr
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import readr


# Read log ----------------------------------------------------------------

read_mtelog <- function(LogFile){

  MainLog <- tibble(Raw = readr::read_lines(LogFile))

  # Exctract date on line 3 of datalogger header
  Date <- str_extract(MainLog[3,], "[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}")

  MainLog %>%
    separate(Raw, into = c("Time", "Instrument", "Data"), sep = c(12,19)) %>%
    mutate(
      DateTime = lubridate::ymd_hms(paste(Date, Time)),
      Instrument = str_extract(Instrument, "[:alnum:]+")
    )
}

# Apla extractor ----------------------------------------------------------

read_apla <- function(MainLog){

  Apla <- MainLog %>%
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

  RawGPGGA <- Apla %>%
    filter(Trame %in% c("GPGGA")
    ) %>%
    pivot_wider(
      names_from = Trame,
      values_from = Data
    ) %>%
    separate(
      col = GPGGA,
      sep = ",",
      into =  c("UTC","Lat","NS","Lon","EW","GPS","Nsat",
                "HorizontalDilution","Altitude","AltitudeUnit",
                "GeoidalSeparation","GeoidalSeparationUnit","GPSAge",
                "DiffID")
    ) %>% # Filter malformated Lat and Lon field
    filter(str_detect(Lat,"[:digit:]{4}\\.[:digit:]{5}") & str_detect(Lon,"[:digit:]{5}\\.[:digit:]{5}")
    ) %>% # Filter incorect N S W E field
    filter(NS %in% c("N","S") & EW %in% c("E","W"))

  # There is some wrong data from time to time (missing field, incomplete or other format Lat Lon), Will have to elucidate that (Applanix or DataLogger issue ?)

  # Could add a filter based on the pattern [:digit:]{4}\.[:digit:]{5} for Lat and [:digit:]{5}\.[:digit:]{5} for Lon

  GPGGA <- RawGPGGA %>%
    mutate(Lat_D = as.numeric(str_sub(Lat, 1, 2)),
           Lat_DM = as.numeric(str_sub(Lat, 3,10)),
           Lon_D = as.numeric(str_sub(Lon, 1, 3)),
           Lon_DM = as.numeric(str_sub(Lon, 4,11)),
           Lat_DD = Lat_D + (Lat_DM/60),
           Lon_DD = Lon_D + (Lon_DM/60)) %>%
    mutate(Lat_DD = ifelse(NS == "N", Lat_DD, -Lat_DD),
           Lon_DD = ifelse(EW == "W", -Lon_DD, Lon_DD))

  # Add default NA value to ObsType for initial map plot
  GPGGA <- GPGGA %>% select(Time, DateTime, Lat_DD, Lon_DD, HorizontalDilution)

  # Extract GPVTG info, course and speed

  RawGPVTG <- Apla %>%
    filter(Trame %in% c("GPVTG")
    ) %>%
    pivot_wider(
      names_from = Trame,
      values_from = Data
    ) %>%
    separate(
      col = GPVTG,
      sep = ",",
      convert = T,
      into =  c(
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

  GPVTG <- RawGPVTG %>%
    mutate(
      Course_TN = as.numeric(Course_TN),
      Course_MN = as.numeric(Course_MN),
      Speed_N = as.numeric(Speed_N),
      Speed_kmh = as.numeric(Speed_kmh)
      )

  # Join with Time at the second
  Apla <- left_join(GPGGA, GPVTG, by = c("Time"))

  Apla <- Apla %>% rename(date = DateTime, lat = Lat_DD, lon = Lon_DD)

  # Solar altitude above the horizon in radian and azimuth in radian from south to west
  PosSol <- suncalc::getSunlightPosition(data = Apla, keep = c("altitude", "azimuth"))

  Apla <- left_join(Apla, PosSol, by = c("date", "lat", "lon")) %>%
    rename(DateTime = date, Lat_DD = lat, Lon_DD = lon, SolAzm = azimuth, SolAlt = altitude) %>%
    mutate(SolAzm = SolAzm * 180/pi + 180, # convert rad to degree and shift to north reference
           SolAlt = SolAlt * 180/pi,
           BoatSolAzm = SolAzm-Course_TN)

  Apla %>%
    filter(
      Speed_N <= 4,
      BoatSolAzm > 0 & BoatSolAzm < 180
    ) %>%
    mutate(
      ID = seq_along(DateTime),
      ObsType = "Unknown",
      ObsName = NA)

}
