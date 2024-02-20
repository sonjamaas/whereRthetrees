# functions to extract forest areas from osm
extractSpecialForest <- function(aoi){
  specialforest <- aoi%>%
    opq()%>%
    add_osm_feature(key="leaf_type",
                    value = c("mixed","coniferous","deciduous")) %>%
    osmdata_sf()

  return(specialforest)
}
