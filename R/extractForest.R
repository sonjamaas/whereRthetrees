#' A function to extract forest areas from osm
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "landuse" and the value "forest".
#' @examples
#' extractForest(aoi)
#'
extractForest <- function(aoi){
  forest <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse" ,
                    value = "forest" ) %>%
    osmdata_sf()

  return(forest)
}
