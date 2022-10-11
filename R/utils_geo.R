#' geo
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# Map zoom and center -----------------------------------------------------

# Get Zoom level and map center from lat lon, adapted from:
# https://community.plotly.com/t/dynamic-zoom-for-mapbox/32658/11

# lat <- GPGGA$Lat_DD
# lon <- GPGGA$Lon_DD

zoom_center <- function(lat=NULL, lon=NULL){
  # Return default
  if (is.null(lat) | is.null(lon)) return(list(zoom = 0, center = c(0,0)))

  # Get the boundary-box
  b_box = list()
  b_box['height'] <- max(lat)-min(lat)
  b_box['width'] <- max(lon)-min(lon)

  #
  dat <- matrix(c(lat,lon), nrow = length(lat))
  dat <- raster::coordinates(dat)
  center <- raster::coordinates(methods::as(raster::extent(sf::st_bbox(sf::st_multipoint(dat))), "SpatialPolygons"))

  b_box[['center']] <- list(lat = median(lat, na.rm = T), lon = median(lon, na.rm = T))

  # get the area of the bounding box in order to calculate a zoom-level
  area = b_box[['height']] * b_box[['width']]

  # * 1D-linear interpolation with numpy:
  # - Pass the area as the only x-value and not as a list, in order to return a scalar as well
  # - The x-points "xp" should be in parts in comparable order of magnitude of the given area
  # - The zpom-levels are adapted to the areas, i.e. start with the smallest area possible of 0
  # which leads to the highest possible zoom value 20, and so forth decreasing with increasing areas
  # as these variables are antiproportional

  zoom <- approx(x = c(0, 5**-10, 4**-10, 3**-10, 2**-10, 1**-10, 1**-5),
                 y = c(20, 15,    14,     13,     12,     7,      5),
                 xout = area)

  return(list(zoom = zoom[[2]], center = center))
}


# MinMaxLatLon ------------------------------------------------------------
# What if we cross the Equator or Greenwich ?
min_geo <- function(coor, ...) {
  if (any(stringr::str_detect(as.character(coor), "-"))) {
    -min(abs(coor), ...)
  } else {
    min(coor, ...)
  }
}

max_geo <- function(coor, ...) {
  if (any(stringr::str_detect(as.character(coor), "-"))) {
    -max(abs(coor), ...)
  } else {
    max(coor, ...)
  }
}
