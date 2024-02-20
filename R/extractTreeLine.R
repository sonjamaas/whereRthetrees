#' A function to extract tree lines from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "natural" and the value "tree_row".
#' @examples
#' extractTreeLine(aoi)
extractTreeLine <- function(aoi){
  treeLine <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "tree_row") %>%
    osmdata_sf()

  return(treeLine)
}
