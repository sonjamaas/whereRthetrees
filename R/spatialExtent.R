#' A function for determining the bounding box with a specific radius
#' around a position
#'
#' @param xcoord A x coordinate as decimal degree.
#' @param ycoord A y coordinate as decimal degree.
#' @param buffer The buffer radius around the position.
#'
#' @returns A vector of the bounding box around the position,
#'     therefore the area of interest.
#'
#' @examples
#' spatialExtent(8.671087, 52.113866, 300)
#' @keywords internal
spatialExtent <- function(xcoord, ycoord, buffer){                              # spatialExtent becomes a function using x and y coordinates as well as the buffer,
  # which is the area in which the nature points will be searched for
  # make bounding box out of coordinates
  coord <- data.frame(x=xcoord, y=ycoord)                                       # coord becomes a dataframe containing the x and y coordinates
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)                       # pov becomes a sf object of coord with a set coordinate system
  aoibbox <- st_bbox(st_buffer(pov, buffer), crs= 4326)                         # aoibbox becomes the bounding box around the pov with the radius of the buffer and a coordinate system
  aoi <- st_as_sfc(aoibbox)                                                     # make aoi a sf object from aoibbox
  aoi <- as.vector(aoi)                                                         # aoi becomes a vector object

  return(aoi)                                                                   # function returns the area of interest, aka the bounding box around the pov with the set radius
}
