# function to extract all features from osm, calculate the closest nature point and plot
whereRthetrees <- function(x,y,buffer){                                         # whereRtheTrees function uses the x and y coordinates as well as the buffer area

  # get spatial variables
  aoi <- spatialExtent(x,y,buffer)                                              # get the aoi

  xylim <- xyLimits(x,y,buffer)                                                 # get the x and y limits
  xlim <- c(xylim[1],xylim[2])
  ylim <- c(xylim[3],xylim[4])

  pov <- getPOV(x,y)                                                            # get the pov

  # extract features from OSM
  streets <- extractStreets(aoi)                                                # extract the osm features for the specific aoi
  river <- extractRiver(aoi)
  forest <- extractForest(aoi)
  specialForest <- extractSpecialForest(aoi)
  wood <- extractWood(aoi)
  grassland <- extractGrassland(aoi)
  garden <- extractGarden(aoi)
  natureReserve <- extractNatureReserve(aoi)
  meadow <- extractMeadow(aoi)
  wine <- extractWine(aoi)
  singleTree <- extractSingleTree(aoi)
  treeLine <- extractTreeLine(aoi)
  park <- extractPark(aoi)

  closestNature <- findNature(streets, river, forest, specialForest, wood,      # find the closest nature point
                              singleTree, treeLine, grassland, meadow, park,
                              garden, natureReserve, pov)

  # calculate shortest route to nature point
  route <- osrmRoute(pov,closestNature, overview = "simplified")                # calculate the shortest route

  # plot the osm data
  plotFinal <- finalPlot(streets, river, forest,                                # get the final plot
                         specialForest, wood, singleTree,
                         treeLine, grassland,
                         meadow, park, garden,
                         natureReserve, pov, xlim, ylim,closestNature, route)

  return(plotFinal)                                                             # function returns the final plot
}
