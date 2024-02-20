#' A function to extract rivers from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "waterway" and the value "river".
#' @examples
#' extractRiver(aoi)
extractRiver <- function(aoi){
  river <- aoi%>%
    opq()%>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()

  return(river)
}
