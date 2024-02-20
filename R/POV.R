
#' A simple function to get the position as sf object
#'
#' @param xcoord A x coordinate as decimal degree.
#' @param ycoord A y coordinate as decimal degree.
#'
#' @returns The position determined by the two coordinates as
#'     sf object in the coordinate system 4326.
#' @export
#'
#' @examples
#' getPOV(8.671087, 52.113866)
getPOV <- function(xcoord, ycoord){                                             # getPOV is a simple function for just returning the POV as sf object

  coord <- data.frame(x=xcoord, y=ycoord)
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
  return(pov)
}
