#' A function to extract single trees from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "natural" and the value "tree".
#' @examples
#' extractSingleTree(aoi)
#' @keywords internal
extractSingleTree <- function(aoi){
  singleTree <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "tree") %>%
    osmdata_sf()

  return(singleTree)
}
