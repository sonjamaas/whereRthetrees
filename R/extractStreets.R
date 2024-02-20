#' A function to extract streets from OSM
#'
#' @param aoi An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).
#' @returns A simple feature collection of OSM-features with the key "highway" and the values:
#'     "motorway", "trunk", "primary", "secondary",
#'     "tertiary", "unclassified", "residential",
#'     "motorway_link", "trunk_link", "primary_link",
#'     "secondary_link", "tertiary_link",
#'     "living_street", "service", "pedestrian",
#'     "track", "bus_guideway", "escape", "raceway",
#'     "road", "busway",
#'     "footway", "bridleway", "steps", "cooridor",
#'     "path", "via_ferrata",
#'     "ladder", "mini_roundabout", "motorway_junction",
#'     "turning_circle".
#' @examples
#' extractStreets(aoi)
#' @keywords internal
extractStreets <- function(aoi){                                                # function to extract all Streets in the area of interest
  streets <- aoi%>%                                                             # variable streets: for the aoi,
    opq()%>%                                                                    # build a new overpass query object
    add_osm_feature(key = "highway",                                            # and add multiple osm features with the key "highway" and all possible values/types
                    value = c("motorway", "trunk", "primary", "secondary",      # Principal tags for the road network, range from the most to least important
                              "tertiary", "unclassified", "residential",
                              "motorway_link", "trunk_link", "primary_link",    # link roads
                              "secondary_link", "tertiary_link",
                              "living_street", "service", "pedestrian",         # Special road types
                              "track", "bus_guideway", "escape", "raceway",
                              "road", "busway",
                              "footway", "bridleway", "steps", "cooridor",      # paths
                              "path", "via_ferrata",
                              "ladder", "mini_roundabout", "motorway_junction", # other highway features
                              "turning_circle"
                    )) %>%
    osmdata_sf()                                                                # make the osm data into sf objects

  return(streets)                                                               # return the streets variable, now containing a sf collection of all streets in the aoi
}
