#' A function to extract wooda areas from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "natural" and the value "wood".
#' @examples
#' extractWood(aoi)
#' @keywords internal
extractWood <- function(aoi){
  wood <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "wood") %>%
    osmdata_sf()

  return(wood)
}
