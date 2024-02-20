# function for POV
getPOV <- function(xcoord, ycoord){                                             # getPOV is a simple function for just returning the POV as sf object

  coord <- data.frame(x=xcoord, y=ycoord)
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
  return(pov)
}
