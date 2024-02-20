#' A function to find the closest point of public nature
#'
#' @param streets A simple feature collection of OSM-features.
#' @param river A simple feature collection of OSM-features.
#' @param forest A simple feature collection of OSM-features.
#' @param specialForest A simple feature collection of OSM-features.
#' @param wood A simple feature collection of OSM-features.
#' @param singleTree A simple feature collection of OSM-features.
#' @param treeLine A simple feature collection of OSM-features.
#' @param grassland A simple feature collection of OSM-features.
#' @param meadow A simple feature collection of OSM-features.
#' @param park A simple feature collection of OSM-features.
#' @param garden A simple feature collection of OSM-features.
#' @param natureReserve A simple feature collection of OSM-features.
#' @param pov A simple feature of type point.
#'
#' @returns A sf object of the closest available public nature
#' @export
#'
#' @examples
#' findNature(streets, river, forest,
#'           specialForest, wood, singleTree,
#'           treeLine, grassland,
#'           meadow, park, garden,
#'           natureReserve, pov)
findNature <- function(streets, river, forest, specialForest, wood, singleTree, # findNature fuction uses the osm features and the pov
                       treeLine, grassland, meadow, park, garden,
                       natureReserve, pov){

  # check if osm_points is empty and if not get distances
  if(nrow(forest$osm_points)==0){                                               # if forest$osm_points is empty, thus has no rows, keep same data structure as if it was not empty:

    distClosestForest <- NA                                                     # assign NA to the distClosestForest variable (distance to the closest forest point)
    nameClosestForest <- NA                                                     # assign NA to the nameClosestForest variable (name of the closest forest point)
    coordClosestForest <- as.list(c(lon=NA,lat=NA))                             # assing an empty list to the coordClosestForest variable
    print("No forest in this area")                                             # give feedback on missing forest

  }else{                                                                        # if it is not empty:
    # get the distance tables from osrmTable
    distForest <- osrmTable(pov,forest$osm_points,                              # use osrmTable function to get distance matrix (distances from pov to the osm points)
                            measure = c('distance'),                            # distance as measurement for the closest point, but duration is also possible
                            osrm.profile = "car")                               # the osrm demo server that I am using is only supporting the "car" profile for routing,
    # even tho the table function is usable with all profiles...
    # get closest forest point
    distForestsorted <- as.list(                                                # make a list that contains the distances of the distance matrix sorted ascending
      distForest$distances[,order(distForest$distances[1,])])
    distClosestForest <- as.numeric(distForestsorted[1])                        # get the distance to the closest point as numeric value
    nameClosestForest <- names(distForestsorted)[1]                             # get the name of the closest point
    coordClosestForest <- distForest$destinations[c(nameClosestForest),]        # get the coordinates of the closest point
  }


  if(nrow(specialForest$osm_points)==0){
    nameClosestSpecialForest <- NA
    distClosestSpecialForest <- NA
    coordClosestSpecialForest <- as.list(c(lon=NA,lat=NA))
    print("No forest specified as broad- or needleleafed in this area")
  }else{
    # get the distance tables from osrmTable
    distSpecialForest <- osrmTable(pov,specialForest$osm_points,
                                   measure = c('distance'),
                                   osrm.profile = "car")
    # get closest special forest point
    distSpecialForestsorted <- as.list(distSpecialForest$distances[,order(distSpecialForest$distances[1,])])
    distClosestSpecialForest <- as.numeric(distSpecialForestsorted[1])
    nameClosestSpecialForest <- names(distSpecialForestsorted)[1]
    coordClosestSpecialForest <- distSpecialForest$destinations[c(nameClosestSpecialForest),]
  }

  if(nrow(wood$osm_points)==0){
    nameClosestWood <- NA
    distClosestWood <- NA
    coordClosestWood <- as.list(c(lon=NA,lat=NA))
    print("No woods in this area")
  }else{
    # get the distance tables from osrmTable
    distWood <- osrmTable(pov,wood$osm_points,
                          measure = c('distance'),
                          osrm.profile = "car")
    # get closest wood point
    distWoodsorted <- as.list(distWood$distances[,order(distWood$distances[1,])])
    distClosestWood <- as.numeric(distWoodsorted[1])
    nameClosestWood <- names(distWoodsorted)[1]
    coordClosestWood <- distWood$destinations[c(nameClosestWood),]
  }

  if(nrow(grassland$osm_points)==0){
    nameClosestGrassland <- NA
    distClosestGrassland <- NA
    coordClosestGrassland <- as.list(c(lon=NA,lat=NA))
    print("No grassland in this area")
  }else{
    # get the distance tables from osrmTable
    distGrassland <- osrmTable(pov,grassland$osm_points,
                               measure = c('distance'),
                               osrm.profile = "car")
    # get closest grassland point
    distGrasslandsorted <- as.list(distGrassland$distances[,order(distGrassland$distances[1,])])
    distClosestGrassland <- as.numeric(distGrasslandsorted[1])
    nameClosestGrassland <- names(distGrasslandsorted)[1]
    coordClosestGrassland <- distGrassland$destinations[c(nameClosestGrassland),]
  }

  if(nrow(garden$osm_points)==0){
    nameClosestGarden <- NA
    distClosestGarden <- NA
    coordClosestGarden <- as.list(c(lon=NA,lat=NA))
    print("No public garden in this area")
  }else{
    # get the distance tables from osrmTable
    distGarden <- osrmTable(pov,garden$osm_points,
                            measure = c('distance'),
                            osrm.profile = "car")
    # get closest garden point
    distGardensorted <- as.list(distGarden$distances[,order(distGarden$distances[1,])])
    distClosestGarden <- as.numeric(distGardensorted[1])
    nameClosestGarden <- names(distGardensorted)[1]
    coordClosestGarden <- distGarden$destinations[c(nameClosestGarden),]
  }

  if(nrow(natureReserve$osm_points)==0){
    nameClosestNatureReserve <- NA
    distClosestNatureReserve <- NA
    coordClosestNatureReserve <- as.list(c(lon=NA,lat=NA))
    print("No nature reserve in this area")
  }else{
    # get the distance tables from osrmTable
    distNatureReserve <- osrmTable(pov,natureReserve$osm_points,
                                   measure = c('distance'),
                                   osrm.profile = "car")
    # get closest garden point
    distNatureReservesorted <- as.list(distNatureReserve$distances[,order(distNatureReserve$distances[1,])])
    distClosestNatureReserve <- as.numeric(distNatureReservesorted[1])
    nameClosestNatureReserve <- names(distNatureReservesorted)[1]
    coordClosestNatureReserve <- distNatureReserve$destinations[c(nameClosestNatureReserve),]
  }

  if(nrow(meadow$osm_points)==0){
    nameClosestMeadow <- NA
    distClosestMeadow <- NA
    coordClosestMeadow <- as.list(c(lon=NA,lat=NA))
    print("No meadow in this area")
  }else{
    # get the distance tables from osrmTable
    distMeadow <- osrmTable(pov,meadow$osm_points,
                            measure = c('distance'),
                            osrm.profile = "car")
    # get closest garden point
    distMeadowsorted <- as.list(distMeadow$distances[,order(distMeadow$distances[1,])])
    distClosestMeadow <- as.numeric(distMeadowsorted[1])
    nameClosestMeadow <- names(distMeadowsorted)[1]
    coordClosestMeadow <- distMeadow$destinations[c(nameClosestMeadow),]
  }

  if(nrow(wine$osm_points)==0){
    nameClosestWine <- NA
    distClosestWine <- NA
    coordClosestWine <- as.list(c(lon=NA,lat=NA))
    print("No wine yard in this area")
  }else{
    # get the distance tables from osrmTable
    distWine <- osrmTable(pov,wine$osm_points,
                          measure = c('distance'),
                          osrm.profile = "car")
    # get closest wine yard point
    distWinesorted <- as.list(distWine$distances[,order(distWine$distances[1,])])
    distClosestWine <- as.numeric(distWinesorted[1])
    nameClosestWine <- names(distWinesorted)[1]
    coordClosestWine <- distWine$destinations[c(nameClosestWine),]
  }

  if(nrow(singleTree$osm_points)==0){
    nameClosestSingleTree <- NA
    distClosestSingleTree <- NA
    coordClosestSingleTree <- as.list(c(lon=NA,lat=NA))
    print("No single trees in this area")
  }else{
    # get the distance tables from osrmTable
    distSingleTree <- osrmTable(pov,singleTree$osm_points,
                                measure = c('distance'),
                                osrm.profile = "car")
    # get closest single tree point
    distSingleTreesorted <- as.list(distSingleTree$distances[,order(distSingleTree$distances[1,])])
    distClosestSingleTree <- as.numeric(distSingleTreesorted[1])
    nameClosestSingleTree <- names(distSingleTreesorted)[1]
    coordClosestSingleTree <- distSingleTree$destinations[c(nameClosestSingleTree),]
  }

  if(nrow(treeLine$osm_points)==0){
    nameClosestTreeLine <- NA
    distClosestTreeLine <- NA
    coordClosestTreeLine <- as.list(c(lon=NA,lat=NA))
    print("No tree lines in this area")
  }else{
    # get the distance tables from osrmTable
    distTreeLine <- osrmTable(pov,treeLine$osm_points,
                              measure = c('distance'),
                              osrm.profile = "car")
    # get closest tree line point
    distTreeLinesorted <- as.list(distTreeLine$distances[,order(distTreeLine$distances[1,])])
    distClosestTreeLine <- as.numeric(distTreeLinesorted[1])
    nameClosestTreeLine <- names(distTreeLinesorted)[1]
    coordClosestTreeLine <- distTreeLine$destinations[c(nameClosestTreeLine),]
  }

  if(nrow(park$osm_points)==0){
    nameClosestPark <- NA
    distClosestPark <- NA
    coordClosestPark <- as.list(c(lon=NA,lat=NA))
    print("No parks in this area")
  }else{
    # get the distance tables from osrmTable
    distPark <- osrmTable(pov,park$osm_points,
                          measure = c('distance'),
                          osrm.profile = "car")
    # get closest park point
    distParksorted <- as.list(distPark$distances[,order(distPark$distances[1,])])
    distClosestPark <- as.numeric(distParksorted[1])
    nameClosestPark <- names(distParksorted)[1]
    coordClosestPark <- distPark$destinations[c(nameClosestPark),]
  }

  # compare all closest class points by putting them into one data frame
  allPoints <- data.frame(Name = c(nameClosestForest,nameClosestSpecialForest,  # allPoints becomes a data frame with the first column containing the names of closest points
                                   nameClosestWood,nameClosestGrassland,        # of each osm feature group
                                   nameClosestGarden,nameClosestNatureReserve,
                                   nameClosestMeadow,nameClosestWine,
                                   nameClosestSingleTree,nameClosestTreeLine,
                                   nameClosestPark),
                          lon = c(coordClosestForest$lon,                       # the second column containing the longitude of the points
                                  coordClosestSpecialForest$lon,
                                  coordClosestWood$lon,
                                  coordClosestGrassland$lon,
                                  coordClosestGarden$lon,
                                  coordClosestNatureReserve$lon,
                                  coordClosestMeadow$lon,
                                  coordClosestWine$lon,
                                  coordClosestSingleTree$lon,
                                  coordClosestTreeLine$lon,
                                  coordClosestPark$lon),
                          lat = c(coordClosestForest$lat,                       # the third column containing the latitude of the points
                                  coordClosestSpecialForest$lat,
                                  coordClosestWood$lat,
                                  coordClosestGrassland$lat,
                                  coordClosestGarden$lat,
                                  coordClosestNatureReserve$lat,
                                  coordClosestMeadow$lat,
                                  coordClosestWine$lat,
                                  coordClosestSingleTree$lat,
                                  coordClosestTreeLine$lat,
                                  coordClosestPark$lat),
                          dist = c(distClosestForest,                           # the fourth column containing the distance of the points
                                   distClosestSpecialForest,distClosestWood,
                                   distClosestGrassland,distClosestGarden,
                                   distClosestNatureReserve,distClosestMeadow,
                                   distClosestWine,distClosestSingleTree,
                                   distClosestTreeLine,distClosestPark))

  # filter the smallest value of the distance column, ignore the NA values
  closestNature <- subset(allPoints,dist== min(allPoints$dist, na.rm=TRUE))     # subsetting the allPoints data frame for the column with the smallest distance, while ignoring the columns with NA values

  # make the subset of the dataframe into a sf object
  closestNature <- st_as_sf(closestNature,coords=c("lon","lat"),crs=4326)       # subset becomes a sf object with a set coordinate system

  return(closestNature)                                                         # function returns the sf object of the closest nature point
}
