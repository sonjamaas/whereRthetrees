#' A function to extract park areas from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "leisure" and the value "park".
#' @examples
#' extractPark(aoi)
extractPark <- function(aoi){
  park <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "park") %>%
    osmdata_sf()

  return(park)
}
