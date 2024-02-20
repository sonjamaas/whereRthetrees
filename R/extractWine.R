extractWine <- function(aoi){
  wine <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse",
                    value = "vineyard") %>%
    osmdata_sf()

  return(wine)
}
