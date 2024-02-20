#' A function to extract meadow areas from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "landuse" and the value "meadow".
#' @examples
#' extractMeadow(aoi)
extractMeadow <- function(aoi){
  meadow <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse",
                    value = "meadow") %>%
    osmdata_sf()

  return(meadow)
}
