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
