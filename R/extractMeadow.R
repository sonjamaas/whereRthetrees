extractMeadow <- function(aoi){
  meadow <- aoi%>%
    opq()%>%
    add_osm_feature(key="landuse",
                    value = "meadow") %>%
    osmdata_sf()

  return(meadow)
}
