# functions to extract leisure areas from osm
extractPark <- function(aoi){
  park <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "park") %>%
    osmdata_sf()

  return(park)
}
