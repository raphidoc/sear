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
#' @return a logical vector (T/F),
#'
#' @export

data_synthesis <- function(x, y, tol = 3) {
  # as.POSIXct(y, tz = "utc")

  # Recursive time matching algorithm splicing in half each time
  half_life <- function(x, y, tol = tol) {
    condition <- T
    y3 <- y

    while (condition) {
      y2 <- y3[length(y3) / 2]

      if (x <= y2) {
        y3 <- y3[1:(length(y3) / 2)]
      } else {
        y3 <- y3[(length(y3) / 2 + 1):length(y3)]
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
#' @description a utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

discretize_time <- function(Maintime) {
  i <- 1
  j <- 1
  while (T) {
    if (length(Maintime) < i + 1) {
      message("This is the end")
      break
    }

    j <- i

    x <- Maintime[i]
    y <- Maintime[j + 1]

    if (interval(x, y) > seconds(10)) {
      i <- j + 1
      message("next next")
      next
    }

    # Step 1, iter until interval < 3 sec
    while (interval(x, y) < seconds(3)) {
      if (length(Maintime) < j + 1) {
        message("This is the end")
        break
      }

      j <- j + 1
      y <- Maintime[j]
    }

    # At this point. this should always be true
    # Step 3 and 4, processing should happen at this stage
    if (!interval(x, y) > seconds(10) & !interval(x, y) < seconds(3)) {
      print(paste(i, interval(x, y)))
    }

    i <- j + 1
  }
}

#' make_ensemble
#'
#' @description Make ensemble group by duration from timestamp
#'
#' @return a vector of ensemble group
#'
#' @noRd

make_ensemble <- function(date_time, duration = 10) {

  # Transform time stamp to group by interval duration
  ensemble = (as.numeric(date_time) %/% duration) * duration

  return(ensemble)
}

#' gen_metadata
#'
#' @description generate metadata for a discrete obs from MainLog
#'
#' @return a tibble with one row and cols:
#' \itemize{
#'  \item{"date_time"}{}
#'  \item{"date_time_min"}{}
#'  \item{"date_time_max"}{}
#'  \item{"time_elapsed"}{}
#'  \item{"lon"}{}
#'  \item{"lat"}{}
#' }
#'
#' @noRd

gen_metadataL2 <- function(metadata, ensemble) {
  metadata_l2 <- tibble(
    ensemble = ensemble,
    date_time = as.character(format(mean(metadata$date_time, na.rm = T), "%Y-%m-%d %H:%M:%S")),
    date_time_min = as.character(min(metadata$date_time, na.rm = T)),
    date_time_max = as.character(max(metadata$date_time, na.rm = T)),
    time_elapsed = as.numeric(interval(date_time_min, date_time_max)), # in second
    speed = as.numeric(mean(metadata$speed_kmh, na.rm = T)), # speed in kmh
    lon = mean(metadata$lon, na.rm = T),
    lat = mean(metadata$lat, na.rm = T),
    lon_min = min_geo(metadata$lon, na.rm = T),
    lon_max = max_geo(metadata$lon, na.rm = T),
    lat_min = min_geo(metadata$lat, na.rm = T),
    lat_max = max_geo(metadata$lat, na.rm = T),
    distance_run = pracma::haversine(c(lat_min, lon_min), c(lat_max, lon_max)) * 1000, # in meter
    altitude = mean(as.numeric(metadata$altitude), na.rm = T),
    sol_zen = mean(metadata$sol_zen, na.rm = T),
    sol_azi = mean(metadata$sol_azi, na.rm = T),
    boat_raa = mean(metadata$boat_raa, na.rm = T),
    roll = mean(metadata$roll, na.rm = T),
    pitch = mean(metadata$pitch, na.rm = T),
    heading = mean(metadata$heading, na.rm = T),
    comment = "NA",
    uuid_l2 = NA
  )

  metadata_l2 <- metadata_l2 %>%
    mutate(
      rotated_body = purrr::pmap(list(heading, pitch, roll), rotate_vessel_frame),
      .before = comment
    ) %>%
    unnest(cols = c(rotated_body))

  return(metadata_l2)
}

#' gen_metadataL1b
#'
#' @description generate metadata for a discrete obs from MainLog
#'
#' @return a tibble with one row and cols:
#' \itemize{
#'  \item{"date_time"}{}
#'  \item{"date_time_min"}{}
#'  \item{"date_time_max"}{}
#'  \item{"time_elapsed"}{}
#'  \item{"lon"}{}
#'  \item{"lat"}{}
#' }
#'
#' @noRd

gen_metadataL1b <- function(metadata, ensemble) {
  metadata_l1b <- tibble(
    ensemble = ensemble,
    date_time = as.numeric(metadata$date_time),
    speed = as.numeric(metadata$speed_kmh), # speed in kmh
    lon = metadata$lon,
    lat = metadata$lat,
    altitude = as.numeric(metadata$altitude),
    sol_zen = metadata$sol_zen,
    sol_azi = metadata$sol_azi,
    boat_raa = metadata$boat_raa,
    roll = metadata$roll,
    pitch = metadata$pitch,
    heading = metadata$heading,
    uuid_l2 = NA
  )

  metadata_l1b <- metadata_l1b %>%
    mutate(
      rotated_body = purrr::pmap(list(heading, pitch, roll), rotate_vessel_frame),
      .before = uuid_l2
    ) %>%
    unnest(cols = c(rotated_body))

  return(metadata_l1b)
}

#' qc_shift
#'
#' @description shift selected id between 0 and 1
#'
#' @return The original data frame with the selected id qc value changed
#'
#' @noRd
# qc_shift <- function(df, Selected) {
#   df %>%
#     filter(id == Selected) %>%
#     mutate(qc = if_else(qc == "1", "0", "1")) %>%
#     bind_rows(df %>% filter(id != Selected))
# }

qc_shift <- function(df, Selected) {
  df %>%
    mutate(
      qc = case_when(
        id != Selected ~ qc,
        qc == "1" ~ "0",
        qc == "0" ~ "1"
      )
    )
}

#' QWIP: a Quantitative Metric for Quality Control of Aquatic Reflectance
#' Spectral Shape Using the Apparent Visible wavelength (Dierssen et al., FRS, 2022)
#'
#' @description This function calculates the Quality Water Index Polynomial (QWIP)
#' based on Diesrssen et al 2022 paper.
#'
#' @param Waves wavelengths
#' @param Rrs Remote sensing reflectance
#'
#' @return QWIP.score
#'
#' @author Simon bélanger, Raphael Mabit
#' @export

qc_qwip <- function(Waves, Rrs) {
  # Compute the Apparent Visible wavelength (AVW)
  Waves.int <- (400:700)
  Rrs.int <- spline(Waves, Rrs, xout = Waves.int, method = "natural")$y

  AVW <- sum(Rrs.int) / sum(Rrs.int / Waves.int)

  # Compute Normalized Difference Index (NDI)
  ix.492 <- which(Waves.int == 492)
  ix.560 <- which(Waves.int == 560)
  ix.665 <- which(Waves.int == 665)
  NDI <- (Rrs.int[ix.665] - Rrs.int[ix.492]) / (Rrs.int[ix.665] + Rrs.int[ix.492])

  # Compute the QWIP
  p1 <- -8.399885e-9
  p2 <- 1.715532e-5
  p3 <- -1.301670e-2
  p4 <- 4.357838e0
  p5 <- -5.449532e2
  QWIP <- p1 * (AVW^4) + p2 * (AVW^3) + p3 * (AVW^2) + p4 * AVW + p5

  # QWIP Score
  Score <- NDI - QWIP

  if (abs(Score) < 0.1) Pass <- TRUE else Pass <- FALSE

  predicted.AVW <- 440:600
  predicted.NDI <- p1 * (predicted.AVW^4) +
    p2 * (predicted.AVW^3) +
    p3 * (predicted.AVW^2) +
    p4 * predicted.AVW + p5

  # Classified the spectrum
  if (Rrs.int[ix.665] > Rrs.int[ix.560] | Rrs.int[ix.665] > 0.025) {
    class <- "Red"
    class.col <- "#c80d0d"
  } else {
    if (Rrs.int[ix.560] < Rrs.int[ix.492]) {
      class <- "Blue"
      class.col <- "#0888e0"
    } else {
      class <- "Green"
      class.col <- "#47bf13"
    }
  }

  # Get FU color scale
  # FU <- Rrs2FU(Waves, Rrs)$FU


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
  # }

  return(list("Score" = Score, "Pass" = Pass))
}

#' unique_date_time_second
#'
#' @description remove duplicate time at the second in a data frame
#'
#' @param data a data frame
#'
#' @return a data frame with unique time
#'
#' @noRd

unique_date_time_second <- function(data) {
  nm <- deparse(substitute(data))

  if (length(unique(as.numeric(data$date_time))) == length(as.numeric(data$date_time))) {
    message(paste0("No duplicated second in ", nm))
  } else {
    Nsec <- length(data$date_time[duplicated(data$date_time)])

    warning(
      paste(
        "Removing", Nsec, "duplicated second", nm, "at:",
        paste(data$date_time[duplicated(data$date_time)], collapse = ", ")
      )
    )

    data <- data[!duplicated(data$date_time), ]
  }

  return(data)
}

#' rotate_vessel_frame
#'
#' @description Uses the Tait-Bryan angles (heading, pitch, roll) to rotate the vessel frame
#'
#' @param heading rotation in degree about the Z axis (hand righted cartesian rotation, counterclockwise when vector pointing toward you, 0 = North, 90 = East)
#'
#' @param pitch rotation in degree about the Y axis (hand righted cartesian rotation, counterclockwise when vector pointing toward you)
#'
#' @param roll rotation about the X axis (hand righted cartesian rotation, counterclockwise when vector pointing toward you)
#'
#' @return a data frame with columns
#'  boat_xx, boat_xy, boat_xz, boat_yx, boat_yy, boat_yz, boat_zx, boat_zy, boat_zz
#'  giving the cartesian coordinates of the vessel frame unit vector X, Y, Z.
#'
#' @noRd

rotate_vessel_frame <- function(heading, pitch, roll) {
  # Convert angles from degrees to radians
  heading <- heading * pi / 180
  pitch <- -pitch * pi / 180 # negate pitch nd roll to swhicth between Apllanix coordinate system and Plotly
  roll <- -roll * pi / 180

  # Create the rotation matrices
  RotationZ <- matrix(c(cos(heading), sin(heading), 0, -sin(heading), cos(heading), 0, 0, 0, 1), nrow = 3)
  RotationY <- matrix(c(cos(pitch), 0, -sin(pitch), 0, 1, 0, sin(pitch), 0, cos(pitch)), nrow = 3)
  RotationX <- matrix(c(1, 0, 0, 0, cos(roll), sin(roll), 0, -sin(roll), cos(roll)), nrow = 3)

  # Rotate the body frame in the correct order: Z (heading) -> Y (pitch) -> X (roll)
  RotationMatrix <- RotationZ %*% RotationY %*% RotationX

  # Original body frame vectors (X, Y, Z)
  BodyVectors <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3) # Unit vectors

  # Rotate the body frame vectors
  RotatedVectors <- RotationMatrix %*% BodyVectors

  colnames(RotatedVectors) <- c("boat_x", "boat_y", "boat_z")

  RotatedVectors <- as_tibble(RotatedVectors) %>%
    mutate(
      Coord = c("x", "y", "z")
    ) %>%
    pivot_wider(
      names_from = Coord,
      values_from = c(boat_x, boat_y, boat_z),
      names_sep = ""
    )

  return(RotatedVectors)
}
