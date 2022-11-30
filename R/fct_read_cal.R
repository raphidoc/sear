#' hocr_cal
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
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
  CalData <- CalFile[indi + 1]
  # Put definition and calibration on the same line
  CalFile <- tibble(Def = CalFile, Cal = CalData)
  # Dirty Stuff to remove calibration lines: offset index by one to match calibration lines and logically remove them
  CalFile <- CalFile %>% filter(is.na(!append(indi[-length(indi)], NA, 0)))

  CalFile <- CalFile %>%
    separate(
      col = .data$Def,
      into = c("Type", "ID", "Units", "FieldLength", "DataType", "CalLines", "FitType"),
      sep = " ",
      remove = TRUE,
      convert = TRUE
    )

  CalID <- CalFile %>%
    filter(.data$Type %in% c("INSTRUMENT", "SN")) %>%
    select(.data$Type, .data$ID) %>%
    pivot_wider(
      names_from = .data$Type,
      values_from = .data$ID
    ) %>%
    rename(Instrument = .data$INSTRUMENT)

  OPTIC3 <- CalFile %>%
    filter(.data$Type == "ES" | .data$Type == "LU") %>%
    separate(
      col = .data$Cal,
      into = c("a0", "a1", "im", "cint"),
      sep = " ",
      remove = TRUE,
      convert = TRUE
    ) %>%
    bind_cols(CalID) %>%
    rename(Wavelength = "ID") %>%
    mutate(Wavelength = as.numeric(.data$Wavelength))

  THERM1 <- CalFile %>%
    filter(.data$FitType == "THERM1") %>%
    separate(
      col = .data$Cal,
      into = c("m0", "m1", "m2", "m3", "Tr"),
      sep = " ",
      remove = TRUE,
      convert = TRUE
    ) %>%
    bind_cols(CalID)

  # POLYU span from a0 to an, should write code to take that into account
  # Sep in cal file appear to be two spaces ... cloud manage all those with "[[:blank:]]"
  POLYU <- CalFile %>%
    filter(.data$FitType == "POLYU") %>%
    separate(
      col = .data$Cal,
      into = c("a0", "a1"),
      sep = "[[:blank:]]{2}",
      remove = TRUE,
      convert = TRUE
    ) %>%
    bind_cols(CalID)

  return(list(OPTIC3 = OPTIC3, THERM1 = THERM1, POLYU = POLYU))
}

tidy_cal_hocr <- function(CalFiles) {

  #CalFiles <- list.files(system.file("cal", "hocr", package = "sear"), full.names = TRUE)

  CalList <- purrr::map(CalFiles, read_hocr_cal)

  FlatCal <- unlist(CalList, recursive = FALSE)

  OPTIC3 <- bind_rows(FlatCal[names(FlatCal) == "OPTIC3"])

  THERM1 <- bind_rows(FlatCal[names(FlatCal) == "THERM1"])

  POLYU <- bind_rows(FlatCal[names(FlatCal) == "POLYU"])

  OPTIC3 <- OPTIC3 %>%
    group_by(.data$Instrument, .data$SN) %>%
    nest(OPTIC3 = !matches("Instrument|SN"))

  THERM1 <- THERM1 %>%
    group_by(.data$Instrument, .data$SN) %>%
    nest(THERM1 = !matches("Instrument|SN"))

  INTTIME <- POLYU %>%
    filter(.data$Type == "INTTIME") %>%
    group_by(.data$Instrument, .data$SN) %>%
    nest(INTTIME = !matches("Instrument|SN"))

  SAMPLE <- POLYU %>%
    filter(.data$Type == "SAMPLE") %>%
    group_by(.data$Instrument, .data$SN) %>%
    nest(SAMPLE = !matches("Instrument|SN"))

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

  #CalFile <- sear:::app_sys("cal", "sbe19", "7974.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  CalData <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "Parameter",
        "Value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "Parameter",
      values_from = "Value"
    ) %>%
    mutate(
      TCALDATE = dmy(TCALDATE),
      TA0 = as.numeric(TA0),
      TA1 = as.numeric(TA1),
      TA2 = as.numeric(TA2),
      TA3 = as.numeric(TA3),
      CCALDATE = dmy(CCALDATE),
      CG = as.numeric(CG),
      CH = as.numeric(CH),
      CI = as.numeric(CI),
      CJ = as.numeric(CJ),
      CTCOR = as.numeric(CTCOR),
      CPCOR = as.numeric(CPCOR),
      PCALDATE = dmy(PCALDATE),
      PA0 = as.numeric(PA0),
      PA1 = as.numeric(PA1),
      PA2 = as.numeric(PA2),
      PTCA0 = as.numeric(PTCA0),
      PTCA1 = as.numeric(PTCA1),
      PTCA2 = as.numeric(PTCA2),
      PTCB0 = as.numeric(PTCB0),
      PTCB1 = as.numeric(PTCB1),
      PTCB2 = as.numeric(PTCB2),
      PTEMPA0 = as.numeric(PTEMPA0),
      PTEMPA1 = as.numeric(PTEMPA1),
      PTEMPA2 = as.numeric(PTEMPA2)
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

  #CalFile <- sear:::app_sys("cal", "sbe43", "3625.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  CalData <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "Parameter",
        "Value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "Parameter",
      values_from = "Value"
    ) %>%
    mutate(
      OCALDATE = dmy(OCALDATE),
      SOC = as.numeric(SOC),
      VOFFSET = as.numeric(VOFFSET),
      A = as.numeric(A),
      B = as.numeric(B),
      C = as.numeric(C),
      E = as.numeric(E),
      Tau20 = as.numeric(Tau20),
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

  #CalFile <- sear:::app_sys("cal", "sbe18", "1494.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  CalData <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "Parameter",
        "Value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "Parameter",
      values_from = "Value"
    ) %>%
    mutate(
      PHCALDATE = dmy(PHCALDATE),
      SLOPE = as.numeric(SLOPE),
      OFFSET = as.numeric(OFFSET)
    )
}

#' seaowl_cal
#'
#' @description read SeaOWL cal file
#'
#' @return tibble with calibration data
#'
#' @noRd
read_seaowl_cal <- function(CalFile){

  #CalFile <- sear:::app_sys("cal", "seaowl", "0144.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  CalData <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "Parameter",
        "Value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "Parameter",
      values_from = "Value"
    ) %>%
    mutate(
      CALDATE = dmy(CALDATE),
      BbScaleFactor = as.numeric(BbScaleFactor),
      BbDarkCounts = as.numeric(BbDarkCounts),
      ChlScaleFactor = as.numeric(ChlScaleFactor),
      ChlDarkCounts = as.numeric(ChlDarkCounts),
      OilScaleFactor = as.numeric(OilScaleFactor),
      OilDarkCounts = as.numeric(OilDarkCounts),
      FDOMScaleFactor = as.numeric(FDOMScaleFactor),
      FDOMDarkCounts = as.numeric(FDOMDarkCounts)
    )
}

#' seaowl_cal
#'
#' @description read SeaOWL cal file
#'
#' @return tibble with calibration data
#'
#' @noRd
read_bbfl2_cal <- function(CalFile){

  #CalFile <- sear:::app_sys("cal", "bbfl2", "5745.cal")

  CalRaw <- read_lines(CalFile, skip_empty_rows = T)

  CalData <- tibble(CalRaw) %>%
    separate(
      col = CalRaw,
      sep = "=",
      into = c(
        "Parameter",
        "Value"
      ),
      convert = FALSE
    ) %>%
    pivot_wider(
      names_from = "Parameter",
      values_from = "Value"
    ) %>%
    mutate(
      CalDate = dmy(CalDate),
      NTUScaleFactor = as.numeric(NTUScaleFactor),
      NTUDarkCounts = as.numeric(NTUDarkCounts),
      PEScaleFactor = as.numeric(PEScaleFactor),
      PEDarkCounts = as.numeric(PEDarkCounts),
      PCScaleFactor = as.numeric(PCScaleFactor),
      PCDarkCounts = as.numeric(PCDarkCounts)
    )
}
