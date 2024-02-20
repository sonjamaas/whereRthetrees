#' A function to extract wine yards from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "landuse" and the value "vineyard".
#' @examples
#' extractWine(aoi)
extractWine <- function(aoi){
  wine <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse",
                    value = "vineyard") %>%
    osmdata_sf()

  return(wine)
}
