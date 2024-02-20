#' A function to extract public garden areas from osm
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "leisure" and the value "garden".
#' @examples
#' extractGarden(aoi)
extractGarden <- function(aoi){
  garden <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "garden") %>%
    osmdata_sf()

  return(garden)
}
