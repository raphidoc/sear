#' cal_hocr
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
#'
#' @noRd

read_cal_hocr <- function(CalFiles){
  # CalFiles is a list of calibration files

  # Fit type specific coefficients
  # OPTIC3 fit type: a0, a1, im, cint
  # THERM1 fit type: m0 m1 m2 m3 Tr
  # POLYU fit type: a0 a1

  # Create one tidy data frame per fit type

  CalFile <- readr::read_lines(CalFiles, skip_empty_rows = T)

  CalFile <- CalFile[!str_detect(CalFile, "#")]

  # Cal file have tow types of delimiter ... space for definition line and tab for calibration coefficient

  # Working code over there !
  CalFile <- str_replace_all(CalFile, "\\t", " ")

  # Get index of definition file
  indi <- purrr::imap_dbl(CalFile, ~  ifelse(str_detect(.x, "1 (OPTIC3|THERM1|POLYU)"), .y, NA))
  # Calibration data is on the next line
  CalData <- CalFile[indi+1]
  # Put definition and calibration on the same line
  CalFile <- tibble(Def = CalFile, Cal = CalData)
  # Dirty Stuff to remove calibration lines: offset index by one to match calibration lines and logically remove them
  CalFile <- CalFile %>% filter(is.na(!append(indi[-length(indi)], NA, 0)))

  CalFile <- CalFile %>%
    separate(
      col = Def,
      into = c("Type", "ID", "Units", "FieldLength", "DataType", "CalLines", "FitType"),
      sep = " ",
      remove = T,
      convert = T
    )

  CalID <- CalFile %>%
    filter(Type %in% c("INSTRUMENT", "SN")) %>%
    select(Type, ID) %>%
    pivot_wider(
      names_from = Type,
      values_from = ID) %>%
    rename(Instrument = INSTRUMENT)

  OPTIC3 <- CalFile %>%
    filter(Type == "ES" | Type == "LU") %>%
    separate(
      col = Cal,
      into = c("a0", "a1","im","cint"),
      sep = " ",
      remove = T,
      convert = T
    ) %>%
    bind_cols(CalID) %>%
    rename(Wavelength = "ID") %>%
    mutate(Wavelength = as.numeric(Wavelength))

  THERM1 <- CalFile %>%
    filter(FitType == "THERM1") %>%
    separate(
      col = Cal,
      into = c("m0", "m1","m2","m3","Tr"),
      sep = " ",
      remove = T,
      convert = T
    ) %>%
    bind_cols(CalID)

  # POLYU span from a0 to an, should write code to take that into account
  # Sep in cal file appear to be two spaces ... cloud manage all those with "[[:blank:]]"
  POLYU <- CalFile %>%
    filter(FitType == "POLYU") %>%
    separate(
      col = Cal,
      into = c("a0", "a1"),
      sep = "[[:blank:]]{2}",
      remove = T,
      convert = T
    ) %>%
    bind_cols(CalID)

  #tibble(FitType = c("OPTIC3","THERM1","POLYU"), CalData = list(OPTIC3 = OPTIC3, THERM1 = THERM1, POLYU = POLYU))

  return(list(OPTIC3 = OPTIC3, THERM1 = THERM1, POLYU = POLYU))
}

tidy_cal_hocr <- function(){


  # HOCR calibration data ---------------------------------------------------

  CalFiles <- list.files(system.file("cal","hocr", package = "sear"), full.names = T)

  CalList <- purrr::map(CalFiles, read_cal_hocr)

  FlatCal <- unlist(CalList, recursive = F)

  OPTIC3 <- bind_rows(FlatCal[names(FlatCal) == "OPTIC3"])

  THERM1 <- bind_rows(FlatCal[names(FlatCal) == "THERM1"])

  POLYU <- bind_rows(FlatCal[names(FlatCal) == "POLYU"])

  OPTIC3 <- OPTIC3 %>% group_by(Instrument, SN) %>% nest(OPTIC3 = !matches("Instrument|SN"))

  THERM1 <- THERM1 %>% group_by(Instrument, SN) %>% nest(THERM1 = !matches("Instrument|SN"))

  INTTIME <- POLYU %>% filter(Type == "INTTIME") %>% group_by(Instrument, SN) %>% nest(INTTIME = !matches("Instrument|SN"))

  SAMPLE <- POLYU %>% filter(Type == "SAMPLE") %>% group_by(Instrument, SN) %>% nest(SAMPLE = !matches("Instrument|SN"))


  # List of calibration data by instrument ----------------------------------

  list("HOCR" = list("OPTIC3" = OPTIC3, "THERM1" = THERM1, "INTTIME" = INTTIME, "SAMPLE" = SAMPLE))
}
