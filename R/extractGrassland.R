# functions to extract other nature areas from OSM
extractGrassland <- function(aoi){
  grasslandAndBushes <- aoi%>%
    opq()%>%
    add_osm_feature(key="nature",
                    value = c("grassland","scrub","shrubbery")) %>%
    osmdata_sf()

  return(grasslandAndBushes)
}
