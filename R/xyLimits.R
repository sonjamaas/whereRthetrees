# function to set x and y limits
xyLimits <- function(xcoord, ycoord, buffer){                                   # xyLimits becomes a function using x and y coordinates and the buffer

  # make bounding box out of coordinates
  coord <- data.frame(x=xcoord, y=ycoord)                                       # same variables as in spatialExtend
  pov <- st_as_sf(coord, coords = c("x","y"), crs = 4326)
  aoibbox <- st_bbox(st_buffer(pov, buffer), crs= 4326)

  # set x and y limits
  xlim <- c(aoibbox$xmin, aoibbox$xmax)                                         # x and y limits are calculated from the aoibbox minima and maxima
  ylim <- c(aoibbox$ymin, aoibbox$ymax)

  return(c(xlim,ylim))                                                          # function returns a vector of two variables, which contain the x and y limits of the aoi
}
