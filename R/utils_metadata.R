#' data_synthesis
#'
#' @description Create a data synthesis from two time vectors. Give information on which data have been acquired when.
#'
#' @param x time vector of the 'Main' log, usaly GNSS data).
#'
#' @param y time vector of the instrument for which to create the synthesis
#'
#' @param tol allowed time difference (+-)
#'
#' @return A logical vector (T/F),
#'
#' @export

data_synthesis <- function(x, y, tol = 3) {

  # as.POSIXct(y, tz = "UTC")

  # Recursive time matching algorithm splicing in half each time
  half_life <- function(x, y, tol = tol){

    condition <- T
    y3 <- y

    while (condition){

      y2 <- y3[length(y3)/2]

      if (x <= y2) {
        y3 <- y3[1:(length(y3)/2)]
      } else {
        y3 <- y3[(length(y3)/2+1):length(y3)]
      }

      condition <- length(y3) <= 3
    }

    return(any(near(x, y3, tol = tol)))
  }

  purrr::map_lgl(
    .x = x,
    ~ half_life(as.numeric(.x), as.numeric(y), tol = tol)
  )

}

#' discretize_time
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

discretize_time <- function(MainTime) {
  i = 1
  j = 1
  while (T) {
    if (length(MainTime) < i + 1) {
      message("This is the end")
      break
    }

    j = i

    x = MainTime[i]
    y = MainTime[j + 1]

    if (interval(x, y) > seconds(10)) {
      i = j + 1
      message("next next")
      next
    }

    # Step 1, iter until interval < 3 sec
    while (interval(x, y) < seconds(3)) {
      if (length(MainTime) < j + 1) {
        message("This is the end")
        break
      }

      j = j + 1
      y = MainTime[j]
    }

    # At this point. this should always be true
    # Step 3 and 4, processing should happen at this stage
    if (!interval(x, y) > seconds(10) & !interval(x, y) < seconds(3)) {
      print(paste(i, interval(x, y)))
    }

    i = j + 1
  }
}

#' gen_metadata
#'
#' @description generate metadata for a discrete obs from MainLog
#'
#' @return A tibble with one row and cols:
#' \itemize{
#'  \item{"DateTime"}{}
#'  \item{"DateTimeMin"}{}
#'  \item{"DateTimeMax"}{}
#'  \item{"TimeElapsed"}{}
#'  \item{"Lon"}{}
#'  \item{"Lat"}{}
#' }
#'
#' @noRd

gen_metadataL2 <- function(DateTime = c(), Lon = c(), Lat = c(), Select){

  MetadataL2 <- tibble(
    DateTime = as.character(format(mean(Select$DateTime, na.rm = T) , "%Y-%m-%d %H:%M:%S")),
    DateTimeMin = as.character(min(Select$DateTime, na.rm = T)),
    DateTimeMax = as.character(max(Select$DateTime, na.rm = T)),
    TimeElapsed = as.numeric(interval(DateTimeMin, DateTimeMax)), # in second
    Speed = as.numeric(mean(Select$Speed_kmh, na.rm = T)), # speed in kmh
    Lon = mean(Select$Lon, na.rm = T),
    Lat = mean(Select$Lat, na.rm = T),
    LonMin = min_geo(Select$Lon, na.rm = T),
    LonMax = max_geo(Select$Lon, na.rm = T),
    LatMin = min_geo(Select$Lat, na.rm = T),
    LatMax = max_geo(Select$Lat, na.rm = T),
    DistanceRun = pracma::haversine(c(LatMin, LonMin), c(LatMax, LonMax)) * 1000, # in meter
    Altitude = mean(as.numeric(Select$Altitude), na.rm = T),
    SolZen = mean(Select$SolZen, na.rm = T),
    SolAzm = mean(Select$SolAzm, na.rm = T),
    BoatSolAzm = mean(Select$BoatSolAzm, na.rm = T),
    Roll = mean(Select$Roll, na.rm = T),
    Pitch = mean(Select$Pitch, na.rm = T),
    Heading = mean(Select$Course_TN, na.rm = T),
    Heave = mean(Select$Heave, na.rm = T),
    Comment = "NA",
    UUID = NA
  )

  MetadataL2 <- MetadataL2 %>%
    mutate(
      RotatedBody = purrr::pmap(list(Heading, Pitch, Roll), rotate_vessel_frame),
      .before = Comment
    ) %>% unnest(cols = c(RotatedBody))

  return(MetadataL2)
}

#' gen_metadataL1b
#'
#' @description generate metadata for a discrete obs from MainLog
#'
#' @return A tibble with one row and cols:
#' \itemize{
#'  \item{"DateTime"}{}
#'  \item{"DateTimeMin"}{}
#'  \item{"DateTimeMax"}{}
#'  \item{"TimeElapsed"}{}
#'  \item{"Lon"}{}
#'  \item{"Lat"}{}
#' }
#'
#' @noRd

gen_metadataL1b <- function(DateTime = c(), Lon = c(), Lat = c(), Select){

  MetadataL1b <- tibble(
    DateTime = as.character(format(Select$DateTime, "%Y-%m-%d %H:%M:%S")),
    Speed = as.numeric(Select$Speed_kmh), # speed in kmh
    Lon = Select$Lon,
    Lat = Select$Lat,
    Altitude = as.numeric(Select$Altitude),
    SolZen = Select$SolZen,
    SolAzm = Select$SolAzm,
    BoatSolAzm = Select$BoatSolAzm,
    Roll = Select$Roll,
    Pitch = Select$Pitch,
    Heading = Select$Course_TN,
    Heave = Select$Heave,
    UUID = NA
  )


  MetadataL1b <- MetadataL1b %>%
    mutate(
      RotatedBody = purrr::pmap(list(Heading, Pitch, Roll), rotate_vessel_frame),
      .before = UUID
    ) %>% unnest(cols = c(RotatedBody))

  return(MetadataL1b)
}

#' qc_shift
#'
#' @description shift selected ID between 0 and 1
#'
#' @return The original data frame with the selected ID QC value changed
#'
#' @noRd
# qc_shift <- function(df, Selected) {
#   df %>%
#     filter(ID == Selected) %>%
#     mutate(QC = if_else(QC == "1", "0", "1")) %>%
#     bind_rows(df %>% filter(ID != Selected))
# }

qc_shift <- function(df, Selected) {

  df %>%
    mutate(
      QC = case_when(
        ID != Selected ~ QC,
        QC == "1" ~ "0",
        QC == "0" ~ "1"
      )
    )
}

#' QWIP: A Quantitative Metric for Quality Control of Aquatic Reflectance
#' Spectral Shape Using the Apparent Visible Wavelength (Dierssen et al., FRS, 2022)
#'
#' @description This function calculates the Quality Water Index Polynomial (QWIP)
#' based on Diesrssen et al 2022 paper.
#'
#' @param Waves wavelengths
#' @param Rrs Remote sensing reflectance
#'
#' @return QWIP.score
#'
#' @author Simon Bélanger, Raphael Mabit
#' @export

qc_qwip <- function(Waves, Rrs) {


  # Compute the Apparent Visible Wavelength (AVW)
  Waves.int <- (400:700)
  Rrs.int <- spline(Waves, Rrs, xout=Waves.int, method = "natural")$y

  AVW     <- sum(Rrs.int)/sum(Rrs.int/Waves.int)

  # Compute Normalized Difference Index (NDI)
  ix.492 <- which(Waves.int == 492)
  ix.560 <- which(Waves.int == 560)
  ix.665 <- which(Waves.int == 665)
  NDI     <- (Rrs.int[ix.665] - Rrs.int[ix.492])/(Rrs.int[ix.665] + Rrs.int[ix.492])

  # Compute the QWIP
  p1 <- -8.399885e-9
  p2 <- 1.715532e-5
  p3 <- -1.301670e-2
  p4 <- 4.357838e0
  p5 <- -5.449532e2
  QWIP <- p1*(AVW^4) + p2*(AVW^3) + p3*(AVW^2) + p4*AVW   + p5

  # QWIP Score
  Score = NDI - QWIP

  if (abs(Score) < 0.1) Pass <- TRUE else Pass <- FALSE

  predicted.AVW <- 440:600
  predicted.NDI <- p1*(predicted.AVW^4) +
    p2*(predicted.AVW^3) +
    p3*(predicted.AVW^2) +
    p4*predicted.AVW   + p5

  # Classified the spectrum
  if (Rrs.int[ix.665] > Rrs.int[ix.560] | Rrs.int[ix.665] > 0.025) {
    class = "Red"
    class.col = "#c80d0d"
  } else {
    if (Rrs.int[ix.560] < Rrs.int[ix.492]) {
      class = "Blue"
      class.col = "#0888e0"
    } else {
      class = "Green"
      class.col = "#47bf13"
    }
  }

  # Get FU color scale
  #FU <- Rrs2FU(Waves, Rrs)$FU


  # # Plot
  # if (T) {
  #
  #   df <- data.frame(AVW = predicted.AVW,
  #                    NDI = predicted.NDI,
  #                    NDI.minus.0.1 = predicted.NDI-0.1,
  #                    NDI.plus.0.1 = predicted.NDI+0.1,
  #                    NDI.minus.0.2 = predicted.NDI-0.2,
  #                    NDI.plus.0.2 = predicted.NDI+0.2)
  #
  #   df.rrs <- data.frame(AVW = AVW,
  #                        NDI = NDI,
  #                        class.col = class.col)
  #
  #   dfm <- reshape2::melt(df,id.vars = "AVW")
  #   names(dfm) <- c("AVW", "Predicted", "NDI")
  #
  #   p <- ggplot(dfm, aes(x=AVW, y=NDI)) + geom_line(aes(color=Predicted)) +
  #     geom_point(data=df.rrs, aes(x=AVW, y=NDI, color=class.col),size=3) +
  #     scale_color_manual(name="NDI",labels=c(LABEL, "Predicted", "-0.1", "+0.2", "-0.1", "+0.2"),
  #                        values=c(class.col,"black", "orange",  "red", "orange", "red"))
  #
  #   if (QWIP.PASS) {
  #     p <- p + ggtitle(expression(R[rs]~" passed Quality Control"),
  #                      subtitle = paste("Water class:", class, "      FU color scale:", "FU")) +
  #
  #       labs(caption = "Method from Dierssen et al. Front. Rem. Sens. (2022)") +
  #       theme(plot.title = element_text(size=18, face="bold"),
  #             plot.subtitle = element_text(color = class.col, size=12, face="bold"))
  #   } else  {
  #     p <- p + ggtitle(expression(R[rs]~"Rrs failed Quality Control ; Check extrapolation"),
  #                      subtitle = paste("Water class:", class, "      FU color scale:", "FU")) +
  #       labs(caption = "Method from Dierssen et al. Front. Rem. Sens. (2022)") +
  #       theme(plot.title = element_text(color="#c80d0d", size=18, face="bold.italic"),
  #             plot.subtitle = element_text(color = class.col, size=12, face="bold"))
  #   }
  #
  #   print(p)
  #}

  return(list("Score" = Score, "Pass" = Pass))

}

#' unique_datetime_second
#'
#' @description remove duplicate time at the second in a data frame
#'
#' @param data a data frame
#'
#' @return a data frame with unique time
#'
#' @noRd

unique_datetime_second <- function(data) {

  nm <- deparse(substitute(data))

  if (length(unique(as.numeric(data$DateTime))) == length(as.numeric(data$DateTime))) {

    message(paste0("No duplicated second in ", nm))

  } else {

    data$DateTime[duplicated(data$DateTime)]

    warning(
      paste("Removing duplicated second", nm, "at:",
            paste(data$DateTime[duplicated(data$DateTime)], collapse = ", ")
      )
    )

    data <- data[!duplicated(data$DateTime), ]

  }

  return(data)
}

#' rotate_vessel_frame
#'
#' @description Uses the Tait-Bryan angles (Heading, Pitch, Roll) to rotate the vessel frame
#'
#' @param Heading rotation in degree about the Z axis (hand righted cartesian rotation, counterclockwise when vector pointing toward you, 0 = North, 90 = East)
#'
#' @param Pitch rotation in degree about the Y axis (hand righted cartesian rotation, counterclockwise when vector pointing toward you)
#'
#' @param Roll rotation about the X axis (hand righted cartesian rotation, counterclockwise when vector pointing toward you)
#'
#' @return a data frame with columns
#'  VesselXx, VesselXy, VesselXz, VesselYx, VesselYy, VesselYz, VesselZx, VesselZy, VesselZz
#'  giving the cartesian coordinates of the vessel frame unit vector X, Y, Z.
#'
#' @noRd

rotate_vessel_frame <- function(Heading, Pitch, Roll) {
  # Convert angles from degrees to radians
  Heading <- Heading * pi / 180
  Pitch <- -Pitch * pi / 180 # negate Pitch nd Roll to swhicth between Apllanix coordinate system and Plotly
  Roll <- -Roll * pi / 180

  # Create the rotation matrices
  RotationZ <- matrix(c(cos(Heading), sin(Heading), 0, -sin(Heading), cos(Heading), 0, 0, 0, 1), nrow = 3)
  RotationY <- matrix(c(cos(Pitch), 0, -sin(Pitch), 0, 1, 0, sin(Pitch), 0, cos(Pitch)), nrow = 3)
  RotationX <- matrix(c(1, 0, 0, 0, cos(Roll), sin(Roll), 0, -sin(Roll), cos(Roll)), nrow = 3)

  # Rotate the body frame in the correct order: Z (Heading) -> Y (Pitch) -> X (Roll)
  RotationMatrix <- RotationZ %*% RotationY %*% RotationX

  # Original body frame vectors (X, Y, Z)
  BodyVectors <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3)  # Unit vectors

  # Rotate the body frame vectors
  RotatedVectors <- RotationMatrix %*% BodyVectors

  RotatedVectors <- as_tibble(RotatedVectors) %>%
    mutate(
      Coord = c("x", "y", "z")
    ) %>%
    rename(VesselX = V1, VesselY = V2, VesselZ = V3) %>%
    pivot_wider(
      names_from = Coord,
      values_from = c(VesselX,VesselY,VesselZ),
      names_sep = ""
    )

  return(RotatedVectors)
}
