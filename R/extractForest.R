# functions to extract forest areas from osm
extractForest <- function(aoi){
  forest <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse" ,
                    value = "forest" ) %>%
    osmdata_sf()

  return(forest)
}
