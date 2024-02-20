#' A function to extract nature reserve areas from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "leisure" and the value "nature_reserve".
#' @examples
#' extractNatureReserve(aoi)
#' @keywords internal
extractNatureReserve <- function(aoi){
  natureReserve <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "nature_reserve") %>%
    osmdata_sf()

  return(natureReserve)
}
