#' A function to extract forest specified as mixed, coniferous or deciduous from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "leaf_type" and the values "mixed", "coniferous" and "deciduous".
#' @examples
#' extractSpecialForest(aoi)
extractSpecialForest <- function(aoi){
  specialforest <- aoi%>%
    opq()%>%
    add_osm_feature(key="leaf_type",
                    value = c("mixed","coniferous","deciduous")) %>%
    osmdata_sf()

  return(specialforest)
}
