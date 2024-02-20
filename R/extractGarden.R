# functions to extract public garden areas from osm
extractGarden <- function(aoi){
  garden <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "garden") %>%
    osmdata_sf()

  return(garden)
}
