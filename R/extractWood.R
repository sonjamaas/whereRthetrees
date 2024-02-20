extractWood <- function(aoi){
  wood <- aoi%>%
    opq()%>%
    add_osm_feature(key="natural",
                    value = "wood") %>%
    osmdata_sf()

  return(wood)
}
