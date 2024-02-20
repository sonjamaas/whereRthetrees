extractNatureReserve <- function(aoi){
  natureReserve <- aoi%>%
    opq()%>%
    add_osm_feature(key="leisure",
                    value = "nature_reserve") %>%
    osmdata_sf()

  return(natureReserve)
}
