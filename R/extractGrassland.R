#' A function to extract grassland areas from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "nature" and the values "grassland", "scrub" and "shrubbery".
#' @examples
#' extractGrassland(aoi)
extractGrassland <- function(aoi){
  grasslandAndBushes <- aoi%>%
    opq()%>%
    add_osm_feature(key="nature",
                    value = c("grassland","scrub","shrubbery")) %>%
    osmdata_sf()

  return(grasslandAndBushes)
}
